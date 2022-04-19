package controllers


import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.mvc._
import slick.jdbc.JdbcProfile
import play.api.mvc.Results._

import javax.inject.{Inject, Singleton}
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.PostgresProfile.api._

import java.sql.{Date, SQLException}

@Singleton()
class ExternalApplicationSecuredAction @Inject() (
  val parser: BodyParsers.Default,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment
) extends ActionBuilder[Request, AnyContent] with HasDatabaseConfigProvider[JdbcProfile] {
  import environment._

  def executionContext: ExecutionContext = ec

  def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = {
    val fRequest = Future {
      val token = request.headers.get("Authorization").getOrElse(dummyString).split(" ").last
      token
    }

    def fCheckToken(token: String): Future[Unit] = {
      val qCheckToken =
        sql"""
          select exists(
            select *
            from external_application_token
            where token = $token
          )
           """.as[Boolean].head

      db run qCheckToken map {
        case false => throw PermissionDeniedException
        case _ => ()
      }
    }

    val fResult = for {
      token <- fRequest
      _ <- fCheckToken(token)
      result <- block(request)
    } yield result

    fResult recover {
      case _: SQLException =>
        InternalServerError
      case PermissionDeniedException =>
        Forbidden
      case _ =>
        BadRequest
    }
  }
}
