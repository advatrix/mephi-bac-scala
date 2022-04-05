import play.api.libs.json.{JsArray, JsNull, JsObject, JsValue}
import slick.ast.OptionFold


import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.util.UUID
import scala.annotation.tailrec
import scala.util.parsing.combinator._

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends Arith {
  def analyze(input: String) =
    parseAll(expr, input)
}

val input = "2 * (3 + 7)"
print(ParseExpr.analyze(input))

/* ENTITY TEMPLATE PARSING */

object FieldType extends Enumeration {
  type FieldType = Value
  val IntFieldType, OptIntFieldType, BoolFieldType, OptBoolFieldType,
    FloatFieldType, OptFloatFieldType, UuidFieldType, OptUuidFieldType = Value
}

import FieldType._

case class FieldDescription(messageField: String, fieldName: String, fieldType: FieldType, primaryKey: Boolean)
case class EntityTemplate(tableName: String, fields: List[FieldDescription])

sealed trait EntityTemplateParsingException extends Exception
case class PrimaryKeyIsNotProvidedException(private val absentField: String) extends EntityTemplateParsingException {
  override def toString: String = s"No or not full primary key provided; field $absentField is absent"
}


val enhancedFieldTypeMapping = Map (
  "Int" -> Int,
  "Option[Int]" -> Option[Int],
  "Boolean" -> Boolean,
  "Option[Boolean]" -> Option[Boolean],
  "Float" -> Float,
  "Option[Float]" -> Option[Float],
  "UUID" -> UUID,
  "Option[UUID]" -> Option[UUID]
)



class EntityParser extends JavaTokenParsers {
  protected val fieldTypeMapping: Map[String, FieldType] = Map (
    "Int" -> IntFieldType,
    "Option[Int]" -> OptIntFieldType,
    "Boolean" -> BoolFieldType,
    "Option[Boolean]" -> OptBoolFieldType,
    "Float" -> FloatFieldType,
    "Option[Float]" -> OptFloatFieldType,
    "UUID" -> UuidFieldType,
    "Option[UUID]" -> OptUuidFieldType
  )

  def template: Parser[EntityTemplate] = "table"~>stringLiteral~rep(field) ^^ {
    case tableName~fieldDescriptions =>
      EntityTemplate(tableName, fieldDescriptions)
  }

  def field: Parser[FieldDescription] = stringLiteral~stringLiteral~fieldType~opt("pk") ^^ {
    case messageField~fieldName~fieldType~oPk =>
      FieldDescription(messageField, fieldName, fieldTypeMapping(fieldType), oPk.nonEmpty)
  }

  def fieldType: Parser[String] = fieldTypeMapping.keys.map(literal).reduce(_ | _)
}

def optionToSqlNull(o: Option[_]): String = o match {
  case Some(v) => v.toString
  case None => "null"
}

def convert(o: JsValue, t: FieldType): String = t match {
  case IntFieldType => o.as[Int].toString
  case OptIntFieldType => o.asOpt[Int] map optionToSqlNull
  case FloatFieldType => o.as[Float].toString
  case OptFloatFieldType => o.asOpt[Float] map optionToSqlNull
  case BoolFieldType => o.as[Boolean].toString
  case OptBoolFieldType => o.asOpt[Boolean] map optionToSqlNull
  case UuidFieldType => s"'${o.as[UUID]}'"
  case OptUuidFieldType => o.asOpt[UUID] match { case Some(v) => s"'$v'"; case None => "null"}
}

def insertByTemplate(message: JsValue, template: EntityTemplate) = {
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

  val query =
    sqlu"""
      insert into #$tableName
      #$fieldsQueryPart
      values
      #$valuesQueryPart
       """
}

def updateByTemplate(message: JsValue, template: EntityTemplate) = {
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

  val query =
    sqlu"""
      update #$tableName
      $values
      where
      $conditions
        """

}

def deleteByTemplate(message: JsValue, template: EntityTemplate) = {
  val tableName = template.tableName

  val conditions = template.fields.collect {
    case FieldDescription(messageField, fieldName, fieldType, true) =>
      val value = convert((message \ messageField).as[JsValue], fieldType)
      s"$fieldName = $value"
  } mkString " and "

  val query =
    sqlu"""
      delete from #$tableName
      where $conditions
        """
}

def selectByTemplate(message: JsValue, template: EntityTemplate) = {
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

  val query =
    sql"""
      select #$columns
      from #$tableName
      where $conditions
       """
}
