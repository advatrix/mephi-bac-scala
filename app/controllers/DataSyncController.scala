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
class DataSyncController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val externalApplicationSecuredAction: ExternalApplicationSecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  case object EntityTemplateNotFound extends Exception

  def fGetEntityTemplate(name: String): Future[String] = {
    val qGetEntityTemplate =
      sql"""
        select template from "entity_template" where name = $name
         """.as[String].headOption

    db run qGetEntityTemplate map {
      case Some(template) => template
      case None => throw EntityTemplateNotFound
    }
  }

  def insert: Action[AnyContent] = externalApplicationSecuredAction async { implicit request =>
    val fMessage = Future {
      val body = request.body.asJson.get

      val header = (body \ "header").as[String]
      val message = (body \ "message").as[JsObject]

      (header, message)
    }

    val fResult = for {
      (header, message) <- fMessage
      template <- fGetEntityTemplate(header)
      query = EntityTemplateProcessor.insert(message, template)
      _ <- db run query
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case EntityTemplateNotFound => NotFound
      case ex => BadRequest(ex.toString)
    }
  }

  def update: Action[AnyContent] = externalApplicationSecuredAction async { implicit request =>
    val fMessage = Future {
      val body = request.body.asJson.get

      val header = (body \ "header").as[String]
      val message = (body \ "message").as[JsObject]

      (header, message)
    }

    val fResult = for {
      (header, message) <- fMessage
      template <- fGetEntityTemplate(header)
      query = EntityTemplateProcessor.update(message, template)
      _ <- db run query
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case EntityTemplateNotFound => NotFound
      case ex => BadRequest(ex.toString)
    }
  }

  def read: Action[AnyContent] = externalApplicationSecuredAction async { implicit request =>
    val fMessage = Future {
      val body = request.body.asJson.get

      val header = (body \ "header").as[String]
      val message = (body \ "message").as[JsObject]

      (header, message)
    }

    val fResult = for {
      (header, message) <- fMessage
      template <- fGetEntityTemplate(header)
      query = EntityTemplateProcessor.select(message, template)
      result <- db run query
    } yield Ok {
      Json.toJson(result)
    }

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case EntityTemplateNotFound => NotFound
      case ex => BadRequest(ex.toString)
    }
  }

  def write: Action[AnyContent] = externalApplicationSecuredAction async { implicit request =>
    val fMessage = Future {
      val body = request.body.asJson.get

      val header = (body \ "header").as[String]
      val message = (body \ "message").as[JsObject]

      (header, message)
    }

    val fResult = for {
      (header, message) <- fMessage
      template <- fGetEntityTemplate(header)
      insertQuery = EntityTemplateProcessor.insert(message, template)
      updateQuery = EntityTemplateProcessor.update(message, template)
      _ <- db run (insertQuery andThen updateQuery)
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case EntityTemplateNotFound => NotFound
      case ex => BadRequest(ex.toString)
    }
  }
}
