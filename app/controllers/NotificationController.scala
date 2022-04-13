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
class NotificationController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val securedAction: SecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  def getNotificationsCount: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fNotificationsCount = {
      val qNotificationsCount =
        sql"""
          select
            count(*)
          from "notification_to_user" ntu
          join "notification" n on n.id = ntu.notification_id
          where
            ntu.user_id = $userId
            and not ntu.is_read
           """.as[Int].head

      db run qNotificationsCount
    }

    val fResult = for {
      count <- fNotificationsCount
    } yield Ok {
      Json.obj (
        "count" -> count
      )
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case _ =>
        BadRequest
    }
  }

  def getNotifications(oOffset: Option[Int], oLimit: Option[Int]): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val offset = oOffset getOrElse 0
    val limit = oLimit getOrElse 10

    val fNotifications = {
      val qNotifications =
        sql"""
          select
            n.id,
            n.title,
            n.text,
            n.date,
            ntu.is_read
          from "notification_to_user" ntu
          join "notification" n on n.id = ntu.notification_id
          where ntu.user_id = $userId
          order by n.date desc
          offset $offset
          limit $limit
           """.as[(Int, String, String, Date, Boolean)]

      db run qNotifications
    }

    val fResult = for {
      notifications <- fNotifications
    } yield Ok {
      Json.toJson {
        notifications map {
          case (id, title, text, date, isRead) =>
            Json.obj (
              "id" -> id,
              "title" -> title,
              "text" -> text,
              "date" -> date.toLocalDate,
              "is_read" -> isRead
            )
        }
      }
    } as JSON

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case _ =>
        BadRequest
    }
  }

  def setNotificationRead(): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val notificationId = (body \ "id").as[Int]
      notificationId
    }

    def fSetNotificationRead(id: Int) = {
      val qSetNotificationRead =
        sqlu"""
          update "notification_to_user"
          set
            is_read = true
          where notification_id = $id
            and user_id = $userId
            """

      db run qSetNotificationRead
    }

    val fResult = for {
      notificationId <- fRequest
      _ <- fSetNotificationRead(notificationId)
    } yield Ok

    fResult recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case _ =>
        BadRequest
    }
  }

}
