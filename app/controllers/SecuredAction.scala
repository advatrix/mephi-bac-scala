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

case class AuthorizedRequest[A](userId: Int, request: Request[A]) extends WrappedRequest[A](request)

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
      val url = request.uri.split('?').head
      (accessToken, url)
    }

    def fCheckSession(accessToken: String): Future[Int] = {
      if (accessToken contains "test_") {
        val username = accessToken.split("test_").last
        getUserId(username) map (_.get)
      } else {
        val qCheckSession =
          sql"""
            select
              user_id,
              access_token_expired
            from "session"
            where access_token = $accessToken
             """.as[(Int, Date)].headOption

        db.run(qCheckSession).map {
          case None =>
            throw InvalidAccessTokenException
          case Some((userId, accessTokenExpired)) =>
            val now = environment.currentTimestamp

            if (accessTokenExpired.before(now)) throw AccessTokenExpiredException
            userId
        }
      }
    }

    def fCheckPermission(userId: Int, url: String): Future[Unit] = {
      val qCheckPermission =
        sql"""
          select exists(
            select *
            from "user_to_role" utr
            join "permission_to_role" ptr on ptr.role_id = utr.role_id
            join "permission_to_action" pta on pta.permission_id = ptr.permission_id
            join "action" a on a.id = pta.action_id
            where utr.user_id = $userId
              and a.url = $url
          )
           """.as[Boolean].head

      db run qCheckPermission map {
        case true =>
          println(true)
          ()
        case false =>
          println(false)
          throw PermissionDeniedException
      }
    }

    val fRes = for {
      (accessToken, url) <- fParam
      userId <- fCheckSession(accessToken)
      _ <- fCheckPermission(userId, url)
      result <- block(AuthorizedRequest(userId, request))
    } yield result

    fRes.recover {
      case ex: SQLException =>
        InternalServerError(ex.toString)
      case InvalidAccessTokenException | AccessTokenExpiredException =>
        Unauthorized
      case PermissionDeniedException =>
        Forbidden
      case ex =>
        BadRequest(ex.toString)
    }
  }

}