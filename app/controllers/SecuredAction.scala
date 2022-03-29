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

case class AuthorizedRequest[A](userId: UUID, request: Request[A]) extends WrappedRequest[A](request)

@Singleton
class SecuredAction @Inject() (
  val parser: BodyParsers.Default,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment
) extends ActionBuilder[AuthorizedRequest, AnyContent] with HasDatabaseConfigProvider[JdbcProfile] {

  import environment._

  def executionContext: ExecutionContext = ec

  def invokeBlock[A](request: Request[A], block: AuthorizedRequest[A] => Future[Result]): Future[Result] = {
    val fParam = Future {
      val accessToken = request.headers.get("Authorization").getOrElse(dummyString).split(" ").last
      val url = request.uri.split("/api/").last.split('?').head

      (accessToken, url)
    }

    def fCheckSession(accessToken: String): Future[UUID] = {
      val qCheckSession =
        sql"""
          select
            user_id,
            access_token_expired
          from "session"
          where access_token = $accessToken
           """.as[(UUID, Date)].headOption

      db.run(qCheckSession).map {
        case None =>
          throw InvalidAccessTokenException
        case Some((userId, accessTokenExpired)) =>
          val now = environment.currentTimestamp

          if (accessTokenExpired.before(now)) throw AccessTokenExpiredException
          userId
      }
    }

    def fCheckPermission(userId: UUID, url: String): Future[Boolean] = Future.successful(true)

    val fRes = for {
      (accessToken, url) <- fParam
      userId <- fCheckSession(accessToken)
      _ <- fCheckPermission(userId, url)
      result <- block(AuthorizedRequest(userId, request))
    } yield result

    fRes.recover {
      case _: SQLException =>
        InternalServerError
      case InvalidAccessTokenException | AccessTokenExpiredException =>
        Unauthorized
      case PermissionDeniedException =>
        Forbidden
      case ex =>
        BadRequest(ex.toString)
    }
  }

}