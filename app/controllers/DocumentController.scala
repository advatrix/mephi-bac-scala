package controllers

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.sql.{SQLException, Timestamp}
import java.util.NoSuchElementException
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

@Singleton
class DocumentController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val securedAction: SecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  sealed trait DocumentAction
  case object Read extends DocumentAction
  case object Edit extends DocumentAction
  case object ChangeStatus extends DocumentAction

  type DocumentId = Int

  object DocumentStatus {
    val Init: String = "init"
    val Editing: String = "editing"
    val Pending: String = "pending"
    val Rejected: String = "rejected"
    val Accepted: String = "accepted"
    val Closed: String = "closed"
  }

  object DocumentCommand {
    val Save: String = "save"
    val Send: String = "send"
    val Accept: String = "accept"
    val Reject: String = "reject"
  }

  case class DocumentTransitionException(status: String, command: String) extends Exception {
    override def toString: String = s"Transition $command from state $status is not defined"
  }

  case class DocumentTemplateNotFoundException(name: String) extends Exception {
    override def toString: String = s"Document template $name not found"
  }

  import DocumentParsing.DocumentValidationException

  def documentTransition(state: String, command: String): String = state match {
    case DocumentStatus.Init => command match {
      case DocumentCommand.Save => DocumentStatus.Editing
    }
    case DocumentStatus.Editing => command match {
      case DocumentCommand.Save => DocumentStatus.Editing
      case DocumentCommand.Send => DocumentStatus.Pending
    }
    case DocumentStatus.Pending => command match {
      case DocumentCommand.Reject => DocumentStatus.Rejected
      case DocumentCommand.Accept => DocumentStatus.Accepted
    }
    case DocumentStatus.Rejected => command match {
      case DocumentCommand.Save => DocumentStatus.Editing
    }
    case DocumentStatus.Accepted => command match {
      case DocumentCommand.Save => DocumentStatus.Editing
    }
    case _ => throw DocumentTransitionException(state, command)
  }

  def fCheckDocumentPermission(userId: Int, documentId: DocumentId, action: DocumentAction): Future[Boolean] = {
    val qDocumentPermissionsToUser =
      sql"""
        select
          can_read,
          can_edit,
          can_change_status
        from "document_permission_to_user"
        where user_id = $userId
        and document_id = $documentId
         """.as[(Boolean, Boolean, Boolean)].headOption

    db run qDocumentPermissionsToUser map {
      case None => false
      case Some((canRead, canEdit, canChangeStatus)) => action match {
        case Read => canRead
        case Edit => canEdit
        case ChangeStatus => canChangeStatus
      }
    }
  }


  def getDocuments: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fDocuments = {
      val qDocuments =
        sql"""
          select
            dptu.document_id,
            dt.name,
            dptu.can_read,
            dptu.can_edit,
            dptu.can_change_status,
            ds.name
          from "document_permission_to_user" dptu
          join "document" d on dptu.document_id = d.id
          join "document_template" dt on dt.id = d.template_id
          join "document_status" ds on ds.id = d.status_id
          where dptu.user_id = $userId
           """.as[(DocumentId, String, Boolean, Boolean, Boolean, String)]

      db run qDocuments
    }

    val fResult = for {
      documents <- fDocuments
    } yield Ok {
      Json.toJson (
        documents map {
          case (id, name, canRead, canEdit, canChangeStatus, status) =>
            Json.obj(
              "id" -> id,
              "name" -> name,
              "can_read" -> canRead,
              "can_edit" -> canEdit,
              "can_change_status" -> canChangeStatus,
              "status" -> status
            )
        }
      )
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }

  def getDocumentById(documentId: DocumentId): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fCheckPermission = fCheckDocumentPermission(userId, documentId, Read)

    val fGetDocumentById = fCheckPermission flatMap {
      case false => throw PermissionDeniedException
      case true =>
        val qGetDocumentById =
          sql"""
            select
              dt.name,
              u.username,
              d.user_id,
              ds.name,
              d.content,
              d.comment,
              d.created,
              d.updated
            from "document" d
            join "document_template" dt on dt.id = d.template_id
            join "user" u on u.id = d.user_id
            join "document_status" ds on ds.id = d.status_id
            where d.id = $documentId
             """.as[(String, String, Int, String, Option[String], Option[String], Timestamp, Timestamp)].head

        db run qGetDocumentById
    }

    val fResult = for {
      (name, author, authorId, status, content, comment, created, updated) <- fGetDocumentById
    } yield Ok {
      Json.obj (
        "name" -> name,
        "author" -> author,
        "author_id" -> authorId,
        "status" -> status,
        "content" -> content.map(Json.parse),
        "comment" -> comment,
        "created" -> created.toLocalDateTime,
        "updated" -> updated.toLocalDateTime
      )
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case PermissionDeniedException =>
        Forbidden
      case _: NoSuchElementException =>
        NotFound
      case ex =>
        BadRequest(ex.toString)
    }
  }

  def fGetDocumentTemplate(name: String): Future[(Int, String)] = {
    val qGetDocumentTemplate =
      sql"""
        select id, template from document_template where name = $name
         """.as[(Int, String)].headOption

    db run qGetDocumentTemplate map {
      case Some(template) => template
      case None => throw DocumentTemplateNotFoundException(name)
    }
  }

  def fGetDocumentContext(userId: Int): Future[DocumentContext] = {
    val qGetDocumentContext =
      sql"""
        select
          username,
          email,
          first_name,
          middle_name,
          last_name
        from "user"
        where id = $userId
         """.as[(String, String, String, Option[String], String)].head

    db run qGetDocumentContext map {
      case (username, email, firstName, oMiddleName, lastName) =>
        DocumentContext(username, email, firstName, oMiddleName, lastName)
    }
  }

  def fCreateDocument(userId: Int, templateId: Int): Future[Int] = {
    val qCreateDocument =
      sql"""
        insert into "document" (
          template_id,
          user_id,
          status_id,
          created,
          updated
        )
        select
          $templateId,
          $userId,
          s.id,
          $currentTimestamp,
          $currentTimestamp
        from "document_status" s
        where s.name = ${DocumentStatus.Init}
        returning id
         """.as[Int].head

    db run qCreateDocument
  }

  def fDocumentStatus(documentId: DocumentId): Future[String] = {
    val qDocumentStatus =
      sql"""
        select ds.name
        from "document" d
        join "document_status" ds on ds.id = d.status_id
        where d.id = $documentId
         """.as[String].head

    db run qDocumentStatus
  }

  def qInsertDocumentHistory(
    authorId: Int,
    documentId: DocumentId,
    newStatus: String,
    oldStatus: Option[String] = None,
    comment: Option[String] = None,
    time: Timestamp = currentTimestamp
  ): DBIO[Int] = {
    sql"""
      insert into "document_history" (
        document_id,
        old_status_id,
        new_status_id,
        comment,
        author_id,
        time,
        created,
        updated
      )
      select
        $documentId,
        ods.id,
        nds.id,
        $comment,
        $authorId,
        $time,
        $currentTimestamp,
        $currentTimestamp
      from "document_status" nds
      left join "document_status" ods on ods.name = $oldStatus
      where nds.name = $newStatus
      returning id
        """.as[Int].head
  }

  def fUpdateDocument(
    userId: Int,
    documentId: DocumentId,
    command: String,
    contents: Option[JsObject] = None,
    comment: Option[String] = None
  ): Future[Int] = {
    fDocumentStatus(documentId) flatMap {
      oldStatus =>
        val newStatus = documentTransition(oldStatus, command)

        val qUpdateDocument =
          sql"""
            update "document" d
            set
              content = case when ${contents.nonEmpty} then ${contents.map(_.toString)} else content end,
              comment = case when ${comment.nonEmpty} then $comment else comment end,
              status_id = s.id,
              updated = $currentTimestamp
            from (
              select ds_new.id
              from "document_status" ds_new
              where ds_new.name = $newStatus
            ) s
            where d.id = $documentId
            returning d.id
         """.as[Int].head

        db.run(
          qUpdateDocument
            .andThen(qInsertDocumentHistory(userId, documentId, newStatus, Some(oldStatus), comment))
            .transactionally
        )
    }
  }

  def fSaveDocument(userId: Int, documentId: Int, contents: JsObject): Future[Int] =
    fUpdateDocument(userId, documentId, DocumentCommand.Save, Some(contents))

  def fSendDocument(userId: Int, documentId: DocumentId): Future[Int] =
    fUpdateDocument(userId, documentId, DocumentCommand.Send)

  def fApproveDocument(userId: Int, documentId: DocumentId): Future[Int] =
    fUpdateDocument(userId, documentId, DocumentCommand.Accept)

  def fRejectDocument(userId: Int, documentId: DocumentId, comment: String): Future[Int] =
    fUpdateDocument(userId, documentId, DocumentCommand.Reject, comment = Some(comment))

  def createDocument: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val name = (body \ "name").as[String]
      name
    }

    val fResult = for {
      name <- fRequest
      (templateId, template) <- fGetDocumentTemplate(name)
      context <- fGetDocumentContext(userId)
      document = DocumentTemplateProcessor.createEmptyDocument(template, context)
      id <- fCreateDocument(userId, templateId)
      count <- fSaveDocument(userId, id, document)
    } yield Ok {
      Json.obj(
        "id" -> id,
        "contents" -> document,
        "count" -> count
      )
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }

  def saveDocument: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val id = (body \ "id").as[DocumentId]
      val document = (body \ "document").as[JsObject]
      val name = (body \ "name").as[String]
      (id, document, name)
    }

    val fResult = for {
      (documentId, document, name) <- fRequest
      permission <- fCheckDocumentPermission(userId, documentId, Edit)
      context <-
        if (permission) fGetDocumentContext(userId)
        else Future.failed(PermissionDeniedException)
      (_, template) <- fGetDocumentTemplate(name)
      validationErrors = DocumentTemplateProcessor.validateDocument(document, template, context)
      saved <-
        if (validationErrors.isEmpty) fSaveDocument(userId, documentId, document)
        else Future.failed(DocumentValidationException(validationErrors))
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case PermissionDeniedException => Forbidden
      case _: MatchError => Forbidden("Illegal transition")
      case ex: DocumentValidationException => BadRequest(ex.toString)
      case _: NoSuchElementException => BadRequest("unable to save")
      case ex => BadRequest(ex.toString)
    }
  }

  def sendDocument: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get

      val documentId = (body \ "document_id").as[DocumentId]
      documentId
    }

    val fResult = for {
      documentId <- fRequest
      permission <- fCheckDocumentPermission(userId, documentId, ChangeStatus)
      sent <-
        if (permission) fSendDocument(userId, documentId)
        else Future.failed(PermissionDeniedException)
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case PermissionDeniedException => Forbidden
      case _: MatchError => Forbidden("Illegal transition")
      case ex => BadRequest(ex.toString)
    }
  }

  def approveDocument: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val documentId = (body \ "document_id").as[DocumentId]
      documentId
    }

    val fResult = for {
      documentId <- fRequest
      permission <- fCheckDocumentPermission(userId, documentId, ChangeStatus)
      _ <-
        if (permission) fApproveDocument(userId, documentId)
        else Future.failed(PermissionDeniedException)
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case PermissionDeniedException => Forbidden
      case _: MatchError => Forbidden("Illegal transition")
      case ex => BadRequest(ex.toString)
    }
  }

  def rejectDocument: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get

      val documentId = (body \ "document_id").as[DocumentId]
      val comment = (body \ "comment").as[String]
      (documentId, comment)
    }

    val fResult = for {
      (documentId, comment) <- fRequest
      permission <- fCheckDocumentPermission(userId, documentId, ChangeStatus)
      _ <-
        if (permission) fRejectDocument(userId, documentId, comment)
        else Future.failed(PermissionDeniedException)
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case PermissionDeniedException => Forbidden
      case _: MatchError => Forbidden("Illegal transition")
      case ex => BadRequest(ex.toString)
    }
  }

  def fValidateDocument: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val document = (body \ "document").as[JsObject]
      val name = (body \ "name").as[String]
      (document, name)
    }

    val fResult = for {
      (document, name) <- fRequest
      context <- fGetDocumentContext(userId)
      (_, template) <- fGetDocumentTemplate(name)
      result = DocumentTemplateProcessor.validateDocument(document, template, context)
    } yield Ok {
      Json.arr(result.map(_.toString))
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }

  def getDocumentHistory(id: Int): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    def fGetDocumentHistory = {
      val qGetDocumentHistory =
        sql"""
          select
            dh.old_status_id,
            dh.new_status_id,
            dh.comment,
            dh.author_id,
            dh.time
          from "document_history" dh
          where dh.document_id = $id
           """.as[(Option[Int], Int, Option[String], Int, Option[Timestamp])]

      db run qGetDocumentHistory
    }

    val fResult = for {
      permission <- fCheckDocumentPermission(userId, id, Read)
      history <-
        if (permission) fGetDocumentHistory
        else Future.failed(PermissionDeniedException)
    } yield Ok {
      Json.toJson(
        history map {
          case (oldStatus, newStatus, comment, author, time) =>
            Json.obj (
              "old_status" -> oldStatus,
              "new_status" -> newStatus,
              "comment" -> comment,
              "author" -> author,
              "time" -> time.map(_.toLocalDateTime)
            )
        }
      )
    }

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }
}
