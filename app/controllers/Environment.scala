package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.Configuration
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json._
import play.api.libs.json.Json

import scala.language.implicitConversions
// import play.libs.Json
import slick.jdbc.{GetResult, JdbcProfile, PositionedParameters, SetParameter}
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.PostgresProfile.columnTypes

import java.sql.{Timestamp, Date}
import java.time.{Duration, LocalDate, ZoneId, ZonedDateTime}
import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

@Singleton
class Environment @Inject() (
  val actorSystem: ActorSystem,
  val dbConfigProvider: DatabaseConfigProvider,
  val configuration: Configuration
)(implicit val ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile]
{
  sealed trait UserRegistrationException extends Exception
  case object UsernameAlreadyTakenException extends UserRegistrationException {
    override def toString: String = "username already taken"
  }
  case object EmailAlreadyTakenException extends UserRegistrationException {
    override def toString: String = "email already taken"
  }
  case object UsernameAndEmailAlreadyTakenException extends UserRegistrationException {
    override def toString: String = "username and email already taken"
  }

  sealed trait UserLoginException extends Exception
  case object NoUsernameAndEmailProvidedException extends UserLoginException {
    override def toString: String = "no username and email provided"
  }
  case object UserNotFoundException extends UserLoginException {
    override def toString: String = "user not found"
  }

  sealed trait UserAuthorizationException extends Exception
  case object InvalidAccessTokenException extends UserAuthorizationException {
    override def toString: String = "invalid access token"
  }
  case object AccessTokenExpiredException extends UserAuthorizationException {
    override def toString: String = "access token expired"
  }
  case object PermissionDeniedException extends UserAuthorizationException {
    override def toString: String = "permission denied"
  }

  /* implicit conversions */

  implicit def intToBoolean(value: Int): Boolean = value != 0
  implicit def booleanToInt(value: Boolean): Int = if (value) 1 else 0

  /* UUID - SQL conversion */

  implicit object SetUUID extends SetParameter[UUID] {
    override def apply(v: UUID, pp: PositionedParameters): Unit =
      pp.setObject(v, columnTypes.uuidJdbcType.sqlType)
  }

  implicit object SetUUIDOption extends SetParameter[Option[UUID]] {
    override def apply(v: Option[UUID], pp: PositionedParameters): Unit =
      pp.setObjectOption(v, columnTypes.uuidJdbcType.sqlType)
  }

  implicit val getUUID: GetResult[UUID] = GetResult[UUID](_.nextObject().asInstanceOf[UUID])
  implicit val getUUIDOption: GetResult[Option[UUID]] =
    GetResult[Option[UUID]](_.nextObject().asInstanceOf[Option[UUID]])

  /* TODO: JSON - SQL conversion */

  implicit object SetJSON extends SetParameter[JsValue] {
    override def apply(v: JsValue, pp: PositionedParameters): Unit = ???
  }

  implicit object SetJSONOption extends SetParameter[Option[JsValue]] {
    override def apply(v: Option[JsValue], pp: PositionedParameters): Unit = ???
  }

  // implicit val getJson: GetResult[JsValue] = ???
  // implicit val getJsonOption: GetResult[Option[JsValue]] = ???

  object Settings {
    val accessTokenLength: Int = 20
    val accessTokenDuration: Int = 1
    val refreshTokenLength: Int = 20
    val refreshTokenDuration: Int = 30

    val zoneId: ZoneId = ZoneId.of(configuration.underlying.getString("time_zone"))
    val defaultRoleName: String = "default"
  }

  val dummyString = ""


  // val blockingOpsEc: ExecutionContext = actorSystem.dispatchers.lookup("contexts.blockingOps")

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

  def getUsers: Future[Seq[String]] =
    db run {
      sql"""
        select username from "user"
         """.as[String]
    }

  def getUserId(username: String): Future[Option[Int]] =
    db run {
      sql"""
        select id from "user" where username = $username
         """.as[Int].headOption
    }

  def currentTimestamp: Timestamp = {
    val now = ZonedDateTime.now(Settings.zoneId)
    Timestamp.from(now.toInstant)
  }

  def currentDate: LocalDate = {
    LocalDate.now(Settings.zoneId)
  }

  def registerUser(
    username: String,
    firstName: String,
    oMiddleName: Option[String],
    lastName: String,
    phone: String,
    email: String,
    birthDate: Option[Date],
    password: String
  ): Future[Int] = {
    def fCheckUserExistence: Future[(Boolean, Boolean)] = {
      val qCheckUserExistence =
        sql"""
          select
            exists(select id from "user" where username = $username),
            exists(select id from "user" where email = $email)
           """.as[(Boolean, Boolean)].head

      db run qCheckUserExistence
    }

    fCheckUserExistence flatMap {
      case (false, false) =>
        val now = currentTimestamp
        val created = now
        val updated = now

        val qCreateUser = {
          val qInsertUser =
            sql"""
            insert into "user" (
              username,
              first_name,
              middle_name,
              last_name,
              phone,
              email,
              birth_date,
              password,
              created,
              updated,
              external_id
            ) values (
              $username,
              $firstName,
              $oMiddleName,
              $lastName,
              $phone,
              $email,
              $birthDate,
              $password,
              $created,
              $updated,
              ${UUID.randomUUID}
            )
            returning id
             """.as[Int].head

          qInsertUser flatMap {
            userId =>
              sql"""
                insert into "user_to_role" (
                  user_id,
                  role_id,
                  created,
                  updated
                )
                select
                  $userId,
                  r.id,
                  $created,
                  $updated
                from "role" r
                where r.name = ${Settings.defaultRoleName}
                returning user_id
             """.as[Int].head
          }
        }
        db.run(qCreateUser.transactionally)
      case (true, true) =>
        Future.failed(UsernameAndEmailAlreadyTakenException)
      case (true, false) =>
        Future.failed(UsernameAlreadyTakenException)
      case (false, true) =>
        Future.failed(EmailAlreadyTakenException)
    }
  }

  def fakeAuthorizationService(data: JsObject): Future[JsObject] = {
    def generateRandomToken(length: Int): String = {
      @tailrec
      def generateRandomTokenRec(n: Int, list: List[Char]): List[Char] =
        if (n == 1) Random.nextPrintableChar() :: list
        else generateRandomTokenRec(n-1, Random.nextPrintableChar() :: list)

      generateRandomTokenRec(length, Nil) mkString ""
    }

    val username = (data \ "username").asOpt[String]
    val email = (data \ "email").asOpt[String]
    val password = (data \ "password").as[String]

    val fGetUser = {
      (username, email) match {
        case (None, None) =>
          Future.failed(NoUsernameAndEmailProvidedException)
        case _ =>
          val qGetUser =
            sql"""
              select id from "user"
              where (
                username = $username
                or email = $email
              ) and password = $password
               """.as[Int].headOption

          db run qGetUser map {
            case Some(userId) => userId
            case None => throw UserNotFoundException
          }
      }
    }

    fGetUser flatMap {
      userId =>
        val now = this.currentTimestamp
        val created = now
        val updated = now

        val fakeAccessToken = generateRandomToken(Settings.accessTokenLength)
        val fakeRefreshToken = generateRandomToken(Settings.refreshTokenLength)

        val accessTokenExpired =
          Timestamp.valueOf(currentTimestamp.toLocalDateTime.plusHours(Settings.accessTokenDuration))
        val refreshTokenExpired =
          Timestamp.valueOf(currentTimestamp.toLocalDateTime.plusHours(Settings.refreshTokenDuration))

        val qDeleteSession =
          sqlu"""
            delete from "session"
            where user_id = $userId
             """

        val qCreateSession =
          sql"""
            insert into "session" (
              id,
              user_id,
              access_token,
              access_token_expired,
              refresh_token,
              refresh_token_expired,
              created,
              updated
            ) values (
              ${UUID.randomUUID},
              $userId,
              $fakeAccessToken,
              $accessTokenExpired,
              $fakeRefreshToken,
              $refreshTokenExpired,
              $created,
              $updated
            ) returning id
             """.as[UUID].head

        for {
          _ <- db.run(qDeleteSession andThen qCreateSession)
        } yield play.api.libs.json.Json.obj(
          "access_token" -> fakeAccessToken,
          "refresh_token" -> fakeRefreshToken,
          "access_token_expired" -> accessTokenExpired.toLocalDateTime,
          "refresh_token_expired" -> refreshTokenExpired.toLocalDateTime
        )
    }
  }

  def loginUser(
    data: JsObject,
    externalAuthorizationService: JsObject => Future[JsObject] = fakeAuthorizationService
  ): Future[JsObject] = externalAuthorizationService(data)

  def logoutUser(userId: Int): Future[Unit] = {
    val qLogout =
      sqlu"""
        delete from "session" where user_id = $userId
         """
    db.run(qLogout) map (_ => ())
  }
}
