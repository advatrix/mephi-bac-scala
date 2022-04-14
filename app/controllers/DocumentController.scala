package controllers

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.sql.{Date, SQLException, Timestamp}
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

    db run qGetDocumentTemplate
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
        from "status" s
        where s.name = ${DocumentStatus.Init}
        returning id
         """.as[Int].head

    db run qCreateDocument
  }

  def fSaveDocument(userId: Int, documentId: Int, contents: JsObject): Future[Int] = {
    val qSaveDocument =
      sqlu"""
        update "document" d
        set
          contents = ${contents.toString},
          status_id = s.id,
          updated = $currentTimestamp
        from (
          select id from status where name = ${DocumentStatus.Editing}
        ) s
        where d.id = $documentId and d.user_id = $userId
         """

    db run qSaveDocument
  }

  def createDocument(name: String): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fResult = for {
      (templateId, template) <- fGetDocumentTemplate(name)
      context <- fGetDocumentContext(userId)
      document = DocumentTemplateProcessor.createEmptyDocument(template, context)
      id <- fCreateDocument(userId, templateId)
      _ <- fSaveDocument(userId, id, document)
    } yield Ok {
      Json.obj(
        "id" -> id,
        "contents" -> document
      )
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }

}
