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
class ExamController @Inject() (
  val controllerComponents: ControllerComponents,
  val dbConfigProvider: DatabaseConfigProvider,
  val environment: Environment,
  val securedAction: SecuredAction
) extends BaseController with HasDatabaseConfigProvider[JdbcProfile] {
  import environment._

  def getExamSchedule: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fGetExamSchedule = {
      val qGetExamSchedule =
        sql"""
          select
            sg.name,
            s.name,
            es.id,
            et.name,
            es.start_time,
            es.end_time,
            es.registration_start_time,
            es.registration_end_time,
            es.capacity,
            coalesce(registered.c, 0),
            es.link,
            es.room,
            es.max_points,
            es.pass_points,
            es.description,
            t.id,
            t.personal_link,
            tu.first_name,
            tu.middle_name,
            tu.last_name
          from "study_group" sg
          join user_to_study_group utsg on utsg.study_group_id = sg.id
          join "subject_to_study_group" stsg on stsg.study_group_id = sg.id
          join "subject" s on s.id = stsg.subject_id
          join "subject_to_exam_type" stet on stet.subject_id = s.id
          join "exam_type" et on et.id = stet.exam_type_id
          join "exam_schedule" es on es.subject_exam_type_id = stet.id
          left join (
            select
              count(*) as c,
              utes.exam_schedule_id
            from "user_to_exam_schedule" utes
            group by utes.exam_schedule_id
          ) as registered on registered.exam_schedule_id = es.id
          left join "examiner" e on e.exam_schedule_id = es.id
          left join "tutor" t on e.tutor_id = t.id
          left join "user" tu on tu.id = t.user_id
          where utsg.user_id = $userId
           """.as[(String, (String, ((Int, String, Timestamp, Option[Timestamp], Option[Timestamp], Option[Timestamp],
          Option[Int], Int, Option[String], Option[String], Option[Int], Option[Int], Option[String], Option[Int]),
          (Option[String], Option[String], Option[String], Option[String]))))]

      db.run(qGetExamSchedule) map {
        exams =>
          for ((groupName, groupExams) <- exams.groupMap(_._1)(_._2))
            yield { groupName ->
              (for ((subjectName, subjectExams) <- groupExams.groupMap(_._1)(_._2))
                yield { subjectName ->
                  (for ((_, examScheduleInfo) <- subjectExams.groupBy(_._1._1))
                    yield {
                      examScheduleInfo.groupBy(_._1).map {
                        case (es, tu) =>
                          (es, tu map (_._2))
                      }
                    }
                  )
                }
              )
            }
      }
    }

    val fResult = for {
      schedule <- fGetExamSchedule
    } yield Ok {
      Json.toJson (
        schedule.map {
          case (studyGroup, groupExams) =>
            Json.obj(
              "group" -> studyGroup,
              "exams" -> groupExams.map {
                case (subjectName, subjectExams) =>
                  Json.obj(
                    "subject" -> subjectName,
                    "exams" -> subjectExams.flatMap(_.toSeq).map {
                      case ((esId, examType, startTime, endTime, registrationStartTime, registrationEndTime, capacity,
                      registered, link, room, maxPoints, passPoints, description, _), examiners) =>
                        Json.obj(
                          "id" -> esId,
                          "type" -> examType,
                          "start_time" -> startTime.toLocalDateTime,
                          "end_time" -> endTime.map(_.toLocalDateTime),
                          "registration_start_time" -> registrationStartTime.map(_.toLocalDateTime),
                          "registration_end_time" -> registrationEndTime.map(_.toLocalDateTime),
                          "capacity" -> capacity,
                          "registered" -> registered,
                          "link" -> link,
                          "room" -> room,
                          "max_points" -> maxPoints,
                          "pass_points" -> passPoints,
                          "description" -> description,
                          "examiners" -> examiners.map {
                            case (examinerLink, examinerFirstName, examinerMiddleName, examinerLastName) =>
                              Json.obj(
                                "link" -> examinerLink,
                                "first_name" -> examinerFirstName,
                                "middle_name" -> examinerMiddleName,
                                "last_name" -> examinerLastName
                              )
                          }
                        )

                    }
                  )
              }
            )
        }
      )
    } as JSON

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case ex => BadRequest(ex.toString)
    }
  }

  def setExamSchedule(): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    case object CountUnderCapacityException extends Exception
    case object NoExamPermissionException extends Exception

    val fRequest = Future {
      val body = request.body.asJson.get
      val examScheduleId = (body \ "exam_schedule_id").as[Int]
      examScheduleId
    }

    def fSetExamSchedule(id: Int): Future[Int] = {
      val now = currentTimestamp

      val qCheckExamPermission =
        sql"""
          select exists (
            select *
            from "study_group" sg
            join "user_to_study_group" utsg on utsg.study_group_id = sg.id
            join "subject_to_study_group" stsg on stsg.study_group_id = sg.id
            join "subject_to_exam_type" stet on stet.subject_id = stsg.subject_id
            join "exam_schedule" es on es.subject_exam_type_id = stet.id
            where utsg.user_id = $userId
              and es.id = $id
              and (
                (es.registration_start_time is null and es.registration_end_time >= $now)
                or (es.registration_start_time <= $now and es.registration_end_time is null)
                or ($now between es.registration_start_time and es.registration_end_time)
              )
          )
           """.as[Boolean].head

      val qCheckExamCapacity = qCheckExamPermission flatMap {
        case true =>
          sql"""
            select
              es.capacity is null or es.capacity > count(utes),
              exists(
                select * from user_to_exam_schedule
                join exam_schedule es2 on es2.id = exam_schedule_id
                where es2.subject_exam_type_id = es.subject_exam_type_id
                  and user_id = $userId
              )
              from "exam_schedule" es
              left join "user_to_exam_schedule" utes on utes.exam_schedule_id = es.id
              where es.id = $id
              group by es.id
          """.as[(Boolean, Boolean)].head
        case false => DBIO.failed(NoExamPermissionException)
      }


      val qSetExamSchedule = qCheckExamCapacity flatMap {
        case (true, false) =>
          sql"""
            insert into "user_to_exam_schedule" (
              user_id,
              exam_schedule_id,
              created,
              updated
            ) values (
              $userId,
              $id,
              $currentTimestamp,
              $currentTimestamp
            )
            returning id
             """.as[Int].head
        case _ => DBIO.failed(CountUnderCapacityException)
      }

      db.run(qSetExamSchedule.transactionally)
    }

    val fResult = for {
      examScheduleId <- fRequest
      userToExamScheduleId <- fSetExamSchedule(examScheduleId)
    } yield Ok {
      Json.obj(
        "user_to_exam_schedule_id" -> userToExamScheduleId
      )
    }

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case ex => BadRequest(ex.toString)
    }
  }

  def deleteExamSchedule(): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val userToExamScheduleId = (body \ "user_to_exam_schedule_id").as[Int]
      userToExamScheduleId
    }

    def fDeleteExamSchedule(userToExamScheduleId: Int) = {
      val qDeleteExamSchedule =
        sqlu"""
          delete from "user_to_exam_schedule"
          where user_id = $userId
            and id = $userToExamScheduleId
           """

      db run qDeleteExamSchedule
    }

    val fResult = for {
      userToExamScheduleId <- fRequest
      _ <- fDeleteExamSchedule(userToExamScheduleId)
    } yield Ok

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case ex => BadRequest(ex.toString)
    }
  }

  def getExaminerSchedule: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fExaminerSchedule = {
      val qExaminerSchedule =
        sql"""
          select
            t.id,
            t.occupation,
            es.id,
            et.name,
            s.name,
            es.start_time,
            es.capacity,
            es.description,
            es.link,
            es.room
          from "exam_schedule" es
          join "examiner" e on e.exam_schedule_id = es.id
          join "subject_to_exam_type" stet on stet.id = es.subject_exam_type_id
          join "exam_type" et on et.id = stet.exam_type_id
          join "subject" s on s.id = stet.subject_id
          join "tutor" t on t.id = e.tutor_id
          where t.user_id = $userId
          order by es.start_time desc
           """.as[(Int, String, Int, String, String, Timestamp, Option[Int], Option[String], Option[String], Option[String])]

      db run qExaminerSchedule
    } flatMap {
      examinerSchedule =>
        Future.sequence (
          examinerSchedule.map {
            case (tId, occ, esId, examType, subject, startTime, capacity, description, link, room) =>
              val qStudents =
                sql"""
                  select
                    utes.id,
                    sg.name,
                    u.first_name,
                    u.middle_name,
                    u.last_name
                  from "user_to_exam_schedule" utes
                  join "user" u on utes.user_id = u.id
                  join "user_to_study_group" utsg on utsg.user_id = u.id
                  join "study_group" sg on utsg.study_group_id = utsg.study_group_id
                  where utes.exam_schedule_id = $esId
                   """.as[(Int, String, String, Option[String], String)]

              val qExaminers =
                sql"""
                  select
                    u.first_name,
                    u.last_name,
                    u.middle_name,
                    t.occupation,
                    t.personal_link,
                    t.email,
                    t.room
                  from "tutor" t
                  join "user" u on u.id = t.user_id
                  join "examiner" e on e.tutor_id = t.id
                  where e.exam_schedule_id = $esId
                    and u.id != $userId
                   """.as[(String, String, Option[String], String, Option[String], String, Option[String])]

              for {
                students <- db run qStudents
                examiners <- db run qExaminers
              } yield
                (tId, occ, esId, examType, subject, startTime, capacity, description, link, room, students, examiners)
          }
        )
    }

    val fResult = for {
      schedule <- fExaminerSchedule
    } yield Ok {
      Json.toJson (
        schedule map {
          case (tId, occ, esId, examType, subject, startTime, capacity, description, link, room, students, examiners) =>
            Json.obj(
              "tutor_id" -> tId,
              "occupation" -> occ,
              "exam_schedule_id" -> esId,
              "type" -> examType,
              "subject" -> subject,
              "start_time" -> startTime.toLocalDateTime,
              "capacity" -> capacity,
              "registered" -> students.length,
              "description" -> description,
              "link" -> link,
              "room" -> room,
              "students" -> students.map {
                case (utesId, studyGroup, firstName, middleName, lastName) =>
                  Json.obj(
                    "user_to_exam_schedule_id" -> utesId,
                    "study_group" -> studyGroup,
                    "first_name" -> firstName,
                    "middle_name" -> middleName,
                    "last_name" -> lastName
                  )
              },
              "examiners" -> examiners.map {
                case (firstName, middleName, lastName, occupation, link, email, room) =>
                  Json.obj(
                    "first_name" -> firstName,
                    "middle_name" -> middleName,
                    "last_name" -> lastName,
                    "occupation" -> occupation,
                    "link" -> link,
                    "email" -> email,
                    "room" -> room
                  )
              }
            )
        }
      )
    } as JSON

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case ex => BadRequest(ex.toString)
    }
  }

  def setExamResult(): Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val fRequest = Future {
      val body = request.body.asJson.get
      val userToExamScheduleId = (body \ "user_to_exam_schedule_id").as[Int]
      val result = (body \ "result").as[Int]
      val comment = (body \ "comment").asOpt[String]
      val absence = (body \ "absence").asOpt[Boolean]
      val tutorId = (body \ "tutor_id").as[Int]
      (userToExamScheduleId, tutorId, result, comment, absence)
    }

    def fSetExamResult(utesId: Int, tutorId: Int, result: Int, comment: Option[String], absence: Option[Boolean]): Future[Int] = {
      val now = currentTimestamp

      val qSetExamResult =
        sql"""
          insert into "exam_result" (
            user_to_exam_schedule_id,
            result_points,
            comment,
            absence,
            examiner_id,
            created,
            updated
          )
          select
            $utesId,
            $result,
            $comment,
            $absence,
            t.id,
            $now,
            $now
          from "tutor" t
          where t.id = $tutorId
            and t.user_id = $userId
          returning id
           """.as[Int].head

      db run qSetExamResult
    }

    val fResult = for {
      (userToExamScheduleId, tutorId, result, comment, absence) <- fRequest
      id <- fSetExamResult(userToExamScheduleId, tutorId, result, comment, absence)
    } yield Ok {
      Json.obj("exam_result_id" -> id)
    } as JSON

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case ex => BadRequest(ex.toString)
    }
  }

  def getExamResult: Action[AnyContent] = securedAction async { implicit request =>
    val userId = request.userId

    val qGetExamResult =
      sql"""
        select
          s.name,
          et.name,
          es.start_time,
          es.end_time,
          es.room,
          es.link,
          es.description,
          es.max_points,
          es.pass_points,
          er.result_points,
          er.absence,
          er.comment,
          ex.first_name,
          ex.last_name,
          ex.middle_name
        from "user_to_exam_schedule" utes
        join "exam_schedule" es on utes.exam_schedule_id = es.id
        join "subject_to_exam_type" stet on stet.id = es.subject_exam_type_id
        join "subject" s on s.id = stet.subject_id
        join "exam_type" et on et.id = stet.exam_type_id
        left join "exam_result" er on er.user_to_exam_schedule_id = utes.id
        left join "tutor" t on t.id = er.examiner_id
        left join "user" ex on ex.id = t.user_id
        where utes.user_id = $userId
        order by start_time desc
         """.as[(String, String, Timestamp, Option[Timestamp], Option[String], Option[String], Option[String],
        Option[Int], Option[Int], Option[Int], Option[Boolean], Option[String], Option[String], Option[String],
        Option[String])]

    val fResult = for {
      results <- db run qGetExamResult
    } yield Ok {
      Json.toJson(
        results.map {
          case (subject, examType, startTime, endTime, room, link, description, maxPoints, passPoints, resultPoints,
            absence, comment, firstName, lastName, middleName) =>
            Json.obj(
              "subject" -> subject,
              "exam_type" -> examType,
              "start_time" -> startTime,
              "end_time" -> endTime,
              "room" -> room,
              "link" -> link,
              "description" -> description,
              "max_points" -> maxPoints,
              "pass_points" -> passPoints,
              "result_points" -> resultPoints,
              "absence" -> absence,
              "comment" -> comment,
              "examiner" -> Json.obj(
                "first_name" -> firstName,
                "last_name" -> lastName,
                "middle_name" -> middleName
              )
            )
        }
      )
    } as JSON

    fResult recover {
      case ex: SQLException => InternalServerError(ex.toString)
      case ex => BadRequest(ex.toString)
    }
  }
}
