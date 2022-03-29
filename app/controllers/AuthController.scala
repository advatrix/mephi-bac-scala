package controllers

import play.api.Configuration
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json._
import play.api.mvc._
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.sql.{SQLException, Date}
import java.util.{UUID}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val securedAction: SecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  def signUp: Action[AnyContent] = Action async { implicit request =>
    val fRequest = Future {
      val body = request.body.asJson.get

      val username = (body \ "username").as[String]
      val firstName = (body \ "first_name").as[String]
      val oMiddleName = (body \ "middle_name").asOpt[String]
      val lastName = (body \ "last_name").as[String]
      val phone = (body \ "phone").as[String]
      val email = (body \ "email").as[String]
      val birthDate = (body \ "birth_date").asOpt[Date]

      val password = (body \ "password").as[String]

      (username, firstName, oMiddleName, lastName, phone, email, birthDate, password)
    }

    val fRes = for {
      (username, firstName, oMiddleName, lastName, phone, email, birthDate, password) <- fRequest
      _ <- registerUser(username, firstName, oMiddleName, lastName, phone, email, birthDate, password)
    } yield Ok {
      Json.obj("result" -> "success", "message" -> JsNull)
    }.as(JSON)

    fRes.recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(Json.obj("result" -> "error", "message" -> ex.toString))
    }
  }

  def login(): Action[AnyContent] = Action async { implicit request =>
    val body = request.body.asJson.get.as[JsObject]

    val fRes = for {
      result <- loginUser(body)
    } yield Ok(result).as(JSON)

    fRes.recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }

  def logout(): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId
    val fRes = for {
      _ <- logoutUser(userId)
    } yield Ok

    fRes.recover {
      case _: SQLException =>
        InternalServerError
      case _ =>
        BadRequest
    }
  }
}
