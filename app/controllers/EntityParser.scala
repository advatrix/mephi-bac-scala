package controllers

import FieldType.Value
import play.api.libs.json.{JsArray, JsNull, JsObject, JsValue}
import slick.ast.OptionFold
import slick.jdbc.{GetResult, JdbcProfile}
import slick.jdbc.PostgresProfile.api._

import java.util.UUID
import scala.annotation.tailrec
import scala.util.parsing.combinator._


object FieldType extends Enumeration {
  type FieldType = Value
  val IntFieldType, OptIntFieldType, BoolFieldType, OptBoolFieldType, FloatFieldType, OptFloatFieldType, UuidFieldType,
  OptUuidFieldType, StringFieldType, OptStringFieldType = Value
}

import FieldType._
case class FieldDescription(messageField: String, fieldName: String, fieldType: FieldType, primaryKey: Boolean)
case class EntityTemplate(tableName: String, fields: List[FieldDescription])

sealed trait EntityTemplateParsingException extends Exception
case class PrimaryKeyIsNotProvidedException(private val absentField: String) extends EntityTemplateParsingException {
  override def toString: String = s"No or not full primary key provided; field $absentField is absent"
}
case class GeneralEntityTemplateParsingException(msg: String) extends Exception

case class BadDataMessageException(msg: String) extends Exception


class EntityParser extends JavaTokenParsers {
  def template: Parser[EntityTemplate] = "table"~>tableName~"{"~repsep(field, ",")<~"}" ^^ {
    case tableName~_~fields =>
      EntityTemplate(tableName, fields)
  }
  def tableName: Parser[String] = "[a-zA-Z_]+".r
  def fieldName: Parser[String] = "\""~>"[a-zA-Z0-9а-яА-Я_]+".r<~"\""
  def columnName: Parser[String] = "[a-zA-Z0-9_]+".r
  def field: Parser[FieldDescription] = fieldName~"->"~columnName~fieldType~opt("(?i)pk".r) ^^ {
    case fieldName~_~columnName~columnType~oIsPk =>
      FieldDescription(fieldName, columnName, columnType, oIsPk.nonEmpty)
  }
  def fieldType: Parser[FieldType] =
    int | optInt | bool | optBool | float | optFloat | uuid | optUuid | string | optString
  def int: Parser[FieldType] = "Int" ^^ (_ => IntFieldType)
  def optInt: Parser[FieldType] = "Option[Int]" ^^ (_ => OptIntFieldType)
  def bool: Parser[FieldType] = "Boolean" ^^ (_ => BoolFieldType)
  def optBool: Parser[FieldType] = "Option[Boolean]" ^^ (_ => OptBoolFieldType)
  def float: Parser[FieldType]  = "Float" ^^ (_ => FloatFieldType)
  def optFloat: Parser[FieldType] = "Option[Float]" ^^ (_ => OptFloatFieldType)
  def uuid: Parser[FieldType] = "UUID" ^^ (_ => UuidFieldType)
  def optUuid: Parser[FieldType] = "Option[UUID]" ^^ (_ => OptUuidFieldType)
  def string: Parser[FieldType] = "String" ^^ (_ => StringFieldType)
  def optString: Parser[FieldType] = "Option[String]" ^^ (_ => OptStringFieldType)
}

object EntityTemplateProcessor {
  private object Parser extends EntityParser {
    def parse(input: String): EntityTemplate = parseAll(template, input) match {
      case Success(result, _) => result
      case Failure(msg, next) => throw GeneralEntityTemplateParsingException(s"$msg $next")
      case Error(msg, next) => throw GeneralEntityTemplateParsingException(s"$msg $next")
    }
  }

  private def optionToSqlNull(o: Option[_]): String = o match {
    case Some(v) => v.toString
    case None => "null"
  }

  private def convert(o: JsValue, t: FieldType): String = t match {
    case IntFieldType => o.as[Int].toString
    case OptIntFieldType => optionToSqlNull(o.asOpt[Int])
    case FloatFieldType => o.as[Float].toString
    case OptFloatFieldType => optionToSqlNull(o.asOpt[Float])
    case BoolFieldType => o.as[Boolean].toString
    case OptBoolFieldType => optionToSqlNull(o.asOpt[Boolean])
    case UuidFieldType => s"'${o.as[UUID]}'"
    case OptUuidFieldType => o.asOpt[UUID] match { case Some(v) => s"'$v'"; case None => "null" }
    case StringFieldType => s"'${o.as[String]}'"
    case OptStringFieldType => o.asOpt[String] match { case Some(v) => s"'$v'"; case None => "null" }
  }

  private val sqlKeywords = Set(
    "update", "delete", "insert", "drop", "select", ";", "create"
  )

  def insert(message: JsObject, template: String): DBIO[Int] =
    insertByTemplate(message, Parser.parse(template))

  private def insertByTemplate(message: JsValue, template: EntityTemplate): DBIO[Int] = {
    if (sqlKeywords.exists(message.toString contains _)) throw BadDataMessageException(message.toString)

    val tableName = template.tableName
    val fields = message.as[JsObject].fields
    val messageFields = fields map (_._1)

    def insertQueryParts: (String, String) = {
      @tailrec
      def queryPartsRec(
        fieldsDescriptions: List[FieldDescription],
        fieldsQueryPart: List[String],
        valuesQueryPart: List[String]
      ): (List[String], List[String]) = fieldsDescriptions match {
        case FieldDescription(messageField, fieldName, fieldType, _) :: fds =>
          val value =
            if (!messageFields.contains(messageField)) convert(JsNull, fieldType)
            else convert((message \ messageField).as[JsValue], fieldType)

          queryPartsRec(fds, fieldName :: fieldsQueryPart, value :: valuesQueryPart)
        case Nil =>
          (fieldsQueryPart, valuesQueryPart)
      }
      val (fieldsQueryPart, valuesQueryPart) = queryPartsRec(template.fields, Nil, Nil)
      (fieldsQueryPart.mkString("(", ",", ")"), valuesQueryPart.mkString("(", ",", ")"))
    }

    val (fieldsQueryPart, valuesQueryPart) = insertQueryParts

    sqlu"""
      insert into #$tableName
      #$fieldsQueryPart
      values
      #$valuesQueryPart
    """
  }

  def update(message: JsObject, template: String): DBIO[Int] =
    updateByTemplate(message, Parser.parse(template))

  private def updateByTemplate(message: JsValue, template: EntityTemplate): DBIO[Int] = {
    if (sqlKeywords.exists(message.toString contains _)) throw BadDataMessageException(message.toString)

    val tableName = template.tableName

    val fields = message.as[JsObject].fields
    val messageFields = fields map (_._1)

    def updateQueryParts: (String, String) = {
      @tailrec
      def queryPartsRec(
        fieldsDescriptions: List[FieldDescription],
        valuesQueryPart: List[String],
        conditionsQueryPart: List[String]
      ): (List[String], List[String]) = fieldsDescriptions match {
        case FieldDescription(messageField, fieldName, fieldType, isPk) :: fds =>
          if (isPk) {
            if (!messageFields.contains(messageField)) throw PrimaryKeyIsNotProvidedException(messageField)
            val value = convert((message \ messageField).as[JsValue], fieldType)
            val condition = s"$fieldName = $value"
            queryPartsRec(fds, valuesQueryPart, condition :: conditionsQueryPart)
          } else {
            if (messageFields contains messageField) {
              val value = convert((message \ messageField).as[JsValue], fieldType)
              val assign = s"$fieldName = $value"
              queryPartsRec(fds, assign :: valuesQueryPart, conditionsQueryPart)
            } else
              queryPartsRec(fds, valuesQueryPart, conditionsQueryPart)
          }
        case Nil =>
          (valuesQueryPart, conditionsQueryPart)
      }
      val (valuesQueryPart, conditionsQueryPart) = queryPartsRec(template.fields, Nil, Nil)
      (valuesQueryPart mkString ",", conditionsQueryPart mkString " and ")
    }

    val (values, conditions) = updateQueryParts

    sqlu"""
      update #$tableName
      set #$values
      where #$conditions
    """
  }

  def delete(message: JsObject, template: String): DBIO[Int] =
    deleteByTemplate(message, Parser.parse(template))

  private def deleteByTemplate(message: JsValue, template: EntityTemplate): DBIO[Int] = {
    if (sqlKeywords.exists(message.toString contains _)) throw BadDataMessageException(message.toString)

    val tableName = template.tableName

    val conditions = template.fields.collect {
      case FieldDescription(messageField, fieldName, fieldType, true) =>
        val value = convert((message \ messageField).as[JsValue], fieldType)
        s"$fieldName = $value"
    } mkString " and "

    sqlu"""
      delete from #$tableName
      where #$conditions
    """
  }

  def select(message: JsObject, template: String): DBIO[Seq[Map[String, String]]] =
    selectByTemplate(message, Parser.parse(template))

  private def selectByTemplate(message: JsValue, template: EntityTemplate): DBIO[Seq[Map[String, String]]] = {
    if (sqlKeywords.exists(message.toString contains _)) throw BadDataMessageException(message.toString)

    val tableName = template.tableName

    val fields = message.as[JsObject].fields
    val messageFields = fields map (_._1)

    def selectQueryParts: (String, String) = {
      @tailrec
      def queryPartsRec(
        fieldsDescriptions: List[FieldDescription],
        fieldsQueryPart: List[String],
        conditionsQueryPart: List[String]
      ): (List[String], List[String]) = fieldsDescriptions match {
        case FieldDescription(messageField, fieldName, fieldType, isPk) :: fds =>
          if (isPk) {
            if (!messageFields.contains(messageField)) throw PrimaryKeyIsNotProvidedException(messageField)
            val value = convert((message \ messageField).as[JsValue], fieldType)
            val condition = s"$fieldName = $value"
            queryPartsRec(fds, fieldsQueryPart, condition :: conditionsQueryPart)
          } else queryPartsRec(fds, fieldName :: fieldsQueryPart, conditionsQueryPart)
        case Nil =>
          (fieldsQueryPart, conditionsQueryPart)
      }

      val (fieldsQueryPart, conditions) = queryPartsRec(template.fields, Nil, Nil)
      (fieldsQueryPart mkString ",", conditions mkString " and ")
    }

    val (columns, conditions) = selectQueryParts

    implicit val resultAsStringMap: AnyRef with GetResult[Map[String, String]] = GetResult[Map[String,String]] ( prs =>
      (1 to prs.numColumns).map(_ =>
        prs.rs.getMetaData.getColumnName(prs.currentPos+1) -> prs.nextString
      ).toMap
    )

    sql"""
      select #$columns
      from #$tableName
      where #$conditions
    """.as[Map[String, String]]
  }
}
