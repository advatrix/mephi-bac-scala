package controllers

import play.api.Configuration
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json._
import play.api.mvc._
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.sql.{Date, SQLException}
import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}


@Singleton
class UserController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val securedAction: SecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  def index: Action[AnyContent] = Action async {
    val query =
      sql"""
        select username from "user" limit 10
         """.as[Option[String]]

    val fResult = db run query

    for {
      usernames <- fResult
    } yield Ok {
      Json.arr(usernames)
    } as JSON
  }

  def getUserData: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fUserData = {
      val qUserData =
        sql"""
          select
            username,
            first_name,
            middle_name,
            last_name,
            email,
            birth_date,
            phone
          from "user"
          where id = $userId
           """.as[(String, String, Option[String], String, String, Option[Date], String)].head

      db.run(qUserData)
    }

    val fRes = for {
      (username, firstName, oMiddleName, lastName, email, oBirthDate, phone) <- fUserData
    } yield Ok {
      Json.obj (
        "username" -> username,
        "first_name" -> firstName,
        "middle_name" -> oMiddleName,
        "last_name" -> lastName,
        "email" -> email,
        "birth_date" -> oBirthDate.map(_.toLocalDate),
        "phone" -> phone
      )
    }.as(JSON)

    fRes.recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }

  def updateUserData: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fParam = Future {
      val body = request.body.asJson.get

      val firstName = (body \ "first_name").as[String]
      val middleName = (body \ "middle_name").asOpt[String]
      val lastName = (body \ "last_name").as[String]
      val birthDate = (body \ "birth_date").asOpt[Date]
      val phone = (body \ "phone").as[String]
      val password = (body \ "password").as[String]

      (firstName, middleName, lastName, birthDate, phone, password)
    }

    val fUpdateUserData = fParam.flatMap {
      case (firstName, middleName, lastName, birthDate, phone, password) =>
        val updated = currentTimestamp

        val qUpdateUserData =
          sql"""
            update "user"
            set
              first_name = $firstName,
              middle_name = $middleName,
              last_name = $lastName,
              birth_date = $birthDate,
              phone = $phone,
              password = $password,
              updated = $updated
            where id = $userId
            returning id
             """.as[Int].head

        db.run(qUpdateUserData)
    }

    fUpdateUserData.map (_ => Ok)
      .recover {
        case ex: SQLException =>
          InternalServerError(ex.toString)
        case ex =>
          BadRequest(ex.toString)
      }
  }
}
