package controllers

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.sql.{Date, SQLException}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

@Singleton
class RoleController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val securedAction: SecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  def getPermissions: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fGetPermissions = {
      val qGetPermission =
        sql"""
          select url from "action_permission_to_user" where user_id = $userId
           """.as[String]

      db run qGetPermission
    }

    val fResult = for {
      permissions <- fGetPermissions
    } yield Ok {
      Json.toJson(permissions)
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case ex =>
        BadRequest(ex.toString)
    }
  }
}
