package controllers

import play.api.libs.json._

import scala.language.implicitConversions
import scala.util.parsing.combinator._

object DocumentParsing {
  object UnitType extends Enumeration {
    type UnitType = Value
    val Int, Float, String, Boolean = Value
  }

  sealed trait BracketType
  case object Round extends BracketType
  case object Curly extends BracketType

  sealed trait BinOp
  case object Plus           extends BinOp { override def toString: String = "+" }
  case object Minus          extends BinOp { override def toString: String = "-" }
  case object Multiply       extends BinOp { override def toString: String = "*" }
  case object Divide         extends BinOp { override def toString: String = "/" }
  case object Equal          extends BinOp { override def toString: String = "==" }
  case object NonEqual       extends BinOp { override def toString: String = "!=" }
  case object Greater        extends BinOp { override def toString: String = ">" }
  case object Less           extends BinOp { override def toString: String = "<" }
  case object GreaterOrEqual extends BinOp { override def toString: String = ">=" }
  case object LessOrEqual    extends BinOp { override def toString: String = "<=" }
  case object In             extends BinOp { override def toString: String = "in" }
  case object NotIn          extends BinOp { override def toString: String = "not in" }
  case object Contains       extends BinOp { override def toString: String = "contains" }
  case object NotContains    extends BinOp { override def toString: String = "not contains" }
  sealed trait LogicBinOp extends BinOp
  case object And extends LogicBinOp { override def toString: String = "and" }
  case object Or extends LogicBinOp { override def toString: String = "or" }

  import UnitType.UnitType

  case class DocumentParsingException(msg: String) extends Exception {
    override def toString: String = msg
  }
  case class DocumentExpressionEvaluationException(op: String, ops: Expression*) extends Exception {
    override def toString: String = s"Operation $op is not supported on ${ops.mkString}"
  }
  case class DocumentKeywordEvaluationException(op: String, ops: Any*) extends Exception {
    override def toString: String = s"Keyword $op is not supported with ${ops.mkString}"
  }
  case class DocumentValidationException(errors: Seq[DocumentValidationError]) extends Exception {
    override def toString: String = s"Errors: \n${errors mkString "\n"}"
  }

  sealed trait DocumentValidationError
  case class DocumentRequiredFieldNotFound(field: String) extends DocumentValidationError {
    override def toString: String = s"Required field $field not found"
  }
  case class DocumentWrongFieldType(field: String, expectedType: String) extends DocumentValidationError {
    override def toString: String = s"Field $field is of wrong type, expected type $expectedType"
  }
  case class DocumentFieldLimitNotMet(field: String, limit: String) extends DocumentValidationError {
    override def toString: String = s"Field $field limit $limit is not met"
  }


  sealed trait Expression
  case class BinaryOperation(left: Expression, operator: BinOp, right: Expression) extends Expression {
    override def toString: String = s"$left $operator $right"
  }
  case class Variable(path: List[String]) extends Expression   // refers to document field value
  sealed abstract class Value(val value: Any) extends Expression {
    override def toString: String = value.toString
    def ==(that: Value): BooleanUnitValue = BooleanUnitValue(this.value == that.value)
    def +(that: Value): Value
    def -(that: Value): Value
    def *(that: Value): Value
    def /(that: Value): Value
    def toBooleanUnitValue: BooleanUnitValue = this match {
      case IntUnitValue(0) | FloatUnitValue(0F) | StringUnitValue("") | BooleanUnitValue(false) | Array(Nil) =>
        BooleanUnitValue(false)
      case _ => BooleanUnitValue(true)
    }
    def not: BooleanUnitValue = this.toBooleanUnitValue match {
      case BooleanUnitValue(value) => BooleanUnitValue(!value)
    }
    def or(that: Value): BooleanUnitValue =
      BooleanUnitValue(this.toBooleanUnitValue.value || that.toBooleanUnitValue.value)
    def and(that: Value): BooleanUnitValue =
      BooleanUnitValue(this.toBooleanUnitValue.value && that.toBooleanUnitValue.value)
    def !=(that: Value): BooleanUnitValue = (this == that).not
    def <(that: Value): BooleanUnitValue
    def <=(that: Value): BooleanUnitValue = (this < that) or (this == that)
    def >(that: Value): BooleanUnitValue = (this <= that).not
    def >=(that: Value): BooleanUnitValue = (this > that) or (this == that)
    def in(that: Value): BooleanUnitValue = that match {
      case Array(values) => BooleanUnitValue(values contains this)
      case _ => throw DocumentExpressionEvaluationException("in", this, that)
    }
    def notIn(that: Value): BooleanUnitValue = (this in that).not
    def contains(that: Value): BooleanUnitValue = that in this
    def notContains(that: Value): BooleanUnitValue = (this contains that).not
  }

  final case class IntUnitValue(override val value: Int) extends Value {
    override def +(that: Value): Value = that match {
      case t: IntUnitValue => IntUnitValue(this.value + t.value)
      case t: FloatUnitValue => FloatUnitValue(t.value + this.value)
      case t: BooleanUnitValue => IntUnitValue(this.value + (if (t.value) 1 else 0))
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }
    override def -(that: Value): Value = that match {
      case t: IntUnitValue => IntUnitValue(this.value - t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value - t.value)
      case t: BooleanUnitValue => IntUnitValue(this.value - (if (t.value) 1 else 0))
      case _ => throw DocumentExpressionEvaluationException("-", this, that)
    }
    override def *(that: Value): Value = that match {
      case t: IntUnitValue => IntUnitValue(t.value * this.value)
      case t: FloatUnitValue => FloatUnitValue(t.value * this.value)
      case t: BooleanUnitValue => IntUnitValue(this.value * (if (t.value) 1 else 0))
      case _ => throw DocumentExpressionEvaluationException("*", this, that)
    }
    override def /(that: Value): Value = that match {
      case t: IntUnitValue => IntUnitValue(t.value * this.value)
      case t: FloatUnitValue => FloatUnitValue(t.value * this.value)
      case t: BooleanUnitValue => IntUnitValue(this.value * (if (t.value) 1 else 0))
      case _ => throw DocumentExpressionEvaluationException("*", this, that)
    }
    override def <(that: Value): BooleanUnitValue = that match {
      case t: IntUnitValue => BooleanUnitValue(this.value < t.value)
      case t: FloatUnitValue => BooleanUnitValue(this.value < t.value)
      case _ => throw DocumentExpressionEvaluationException("<", this, that)
    }
  }
  final case class FloatUnitValue(override val value: Float) extends Value {
    override def +(that: Value): FloatUnitValue = that match {
      case t: IntUnitValue => FloatUnitValue(this.value + t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value + t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value + (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }
    override def -(that: Value): FloatUnitValue = that match {
      case t: IntUnitValue => FloatUnitValue(this.value - t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value - t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value - (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("-", this, that)
    }
    override def *(that: Value): FloatUnitValue = that match {
      case t: IntUnitValue => FloatUnitValue(this.value * t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value * t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value * (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("*", this, that)
    }
    override def /(that: Value): FloatUnitValue = that match {
      case t: IntUnitValue => FloatUnitValue(this.value / t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value / t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value / (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("/", this, that)
    }
    override def <(that: Value): BooleanUnitValue = that match {
      case t: IntUnitValue => BooleanUnitValue(this.value < t.value)
      case t: FloatUnitValue => BooleanUnitValue(this.value < t.value)
      case _ => throw DocumentExpressionEvaluationException("<", this, that)
    }
  }
  final case class BooleanUnitValue(override val value: Boolean) extends Value {
    override def +(that: Value): Value = this or that
    override def -(that: Value): Value = throw DocumentExpressionEvaluationException("-", this, that)
    override def *(that: Value): Value = this and that
    override def /(that: Value): Value = throw DocumentExpressionEvaluationException("/", this, that)
    override def <(that: Value): BooleanUnitValue = this and that.not
  }

  final case class StringUnitValue(override val value: String) extends Value {
    override def +(that: Value): StringUnitValue = that match {
      case t: StringUnitValue => StringUnitValue(this.value + t.value)
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }
    override def -(that: Value): Value = throw DocumentExpressionEvaluationException("-", this, that)
    override def *(that: Value): StringUnitValue = that match {
      case t: IntUnitValue => StringUnitValue(this.value * t.value)
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }
    override def /(that: Value): Value = throw DocumentExpressionEvaluationException("/", this, that)
    override def <(that: Value): BooleanUnitValue = throw DocumentExpressionEvaluationException("order", this, that)
    override def in(that: Value): BooleanUnitValue = that match {
      case t: StringUnitValue => BooleanUnitValue(t.value contains this.value)
      case Array(values) => BooleanUnitValue(values contains this)
      case _ => throw DocumentExpressionEvaluationException("in", this, that)
    }
  }

  final case class Array(override val value: List[Value]) extends Value {
    override def +(that: Value): Value = that match {
      case Array(thatValues) => Array(this.value ::: thatValues)
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }
    override def -(that: Value): Value = throw DocumentExpressionEvaluationException("-", this, that)
    override def *(that: Value): Value = throw DocumentExpressionEvaluationException("*", this, that)
    override def /(that: Value): Value = throw DocumentExpressionEvaluationException("/", this, that)
    override def <(that: Value): BooleanUnitValue = throw DocumentExpressionEvaluationException("order", this, that)
    override def in(that: Value): BooleanUnitValue = that match {
      case t: Array => BooleanUnitValue(t.value contains this.value)
      case _ => throw DocumentExpressionEvaluationException("in", this, that)
    }
  }

  case object Empty extends Value {
    override def toString: String = "()"
    override def +(that: Value): Value = that
    override def -(that: Value): Value = that
    override def *(that: Value): Value = throw DocumentExpressionEvaluationException("*", this, that)
    override def /(that: Value): Value = throw DocumentExpressionEvaluationException("/", this, that)
    override def <(that: Value): BooleanUnitValue = throw DocumentExpressionEvaluationException("order", this, that)
    override def in(that: Value): BooleanUnitValue = BooleanUnitValue(true)
  }

  implicit def BooleanUnitValueToBoolean(value: Value): Boolean = value.toBooleanUnitValue.value
  implicit def BooleanToBooleanUnitValue(bool: Boolean): BooleanUnitValue = BooleanUnitValue(bool)

  sealed trait PredefinedConstant extends Expression
  case object Username extends PredefinedConstant { override def toString: String = "username" }
  case object Length extends PredefinedConstant { override def toString: String = "length" }
  case object Self extends PredefinedConstant { override def toString: String = "_" }

  sealed trait FieldSettings
  case class UnitFieldSettings(default: Option[Expression], limits: List[Expression])
  case class ArrayFieldSettings(default: Option[Expression], limits: List[Expression])
  case class ObjectFieldSettings(properties: List[Field])

  sealed trait Field { val name: Option[String] }
  // name is None if field is element of array
  case class UnitField(name: Option[String], fieldType: UnitType.UnitType, required: Boolean, settings: Option[UnitFieldSettings]) extends Field
  case class ArrayField(name: Option[String], required: Boolean, settings: Option[ArrayFieldSettings], elements: Option[Field]) extends Field
  case class ObjectField(name: Option[String], required: Boolean, settings: Option[ObjectFieldSettings]) extends Field

  case class Template(name: String, fields: List[Field])
}

import DocumentParsing._

class DocumentParser extends JavaTokenParsers with PackratParsers {
  val Required: String = "required"
  val Object: String = "object"

  import UnitType.UnitType

  private def getRightParser[U, V](p: Option[U ~ V]): Option[V] =
    p match {
      case Some(_~v) => Some(v)
      case None => None
    }

  private def trim(literal: String): String = literal.substring(1, literal.length - 1)

  def template: Parser[Template] = "document"~>stringLiteral~"{"~repsep(field, ",")<~"}" ^^ {
    case name~_~fields =>
      Template(name.replace("\"", ""), fields)
  }

  lazy val field: PackratParser[Field] = fieldName~(
    arrayFieldDefinition | objectFieldDefinition | unitFieldDefinition
    ) ^^ {
    case name~fieldDefinition =>
      fieldDefinition match {
        case "array"~(oElement: Option[Field])~(oRequired: Option[String])~(oArrayFieldSettings: Option[ArrayFieldSettings]) =>
          ArrayField(Some(name), oRequired.nonEmpty, oArrayFieldSettings, oElement)
        case "object"~(oRequired: Option[String])~(oObjectFieldSettings: Option[ObjectFieldSettings]) =>
          ObjectField(Some(name), oRequired.nonEmpty, oObjectFieldSettings)
        case (fieldType: UnitType)~(oRequired: Option[String])~(oUnitFieldSettings: Option[UnitFieldSettings]) =>
          UnitField(Some(name), fieldType, oRequired.nonEmpty, oUnitFieldSettings)
        case _ =>
          throw DocumentParsingException(s"$name field is defined wrong")
      }
  }

  def fieldName: Parser[String] = "\""~>"[a-zA-Z0-9а-яА-Я_]+".r<~"\""

  lazy val arrayFieldDefinition: PackratParser[String ~ Option[Field] ~ Option[String] ~ Option[ArrayFieldSettings]] =
    "array"~opt(arrayElements)~opt(Required)~opt(arrayFieldSettings)

  def objectFieldDefinition: Parser[String ~ Option[String] ~ Option[ObjectFieldSettings]] =
    "object"~opt(Required)~opt(objectFieldSettings)

  def unitFieldDefinition: Parser[UnitType ~ Option[String] ~ Option[UnitFieldSettings]] =
    unitType~opt(Required)~opt(unitFieldSettings)

  def unitType: Parser[UnitType] = intUnitType | floatUnitType | booleanUnitType | stringUnitType

  def intUnitType: Parser[UnitType] = "int" ^^ (_ => UnitType.Int)
  def floatUnitType: Parser[UnitType] = "float" ^^ (_ => UnitType.Float)
  def booleanUnitType: Parser[UnitType] = "boolean" ^^ (_ => UnitType.Boolean)
  def stringUnitType: Parser[UnitType] = "string" ^^ (_ => UnitType.String)

  lazy val arrayFieldSettings: PackratParser[ArrayFieldSettings] = oBracket~opt(default)~opt(","~limits)~cBracket ^^ {
    case oBracket~oDefault~oLimits~cBracket if oBracket == cBracket =>
      val limits = getRightParser(oLimits) getOrElse List.empty[Expression]
      ArrayFieldSettings(oDefault, limits)
    case _ =>
      throw DocumentParsingException(s"in array field brackets don\'t match: $oBracket and $cBracket")
  }

  lazy val arrayElements: PackratParser[Field] = " of "~>(
    arrayFieldDefinition | objectFieldDefinition | unitFieldDefinition
  ) ^^ {
    case "array"~(oElement: Option[Field])~(oRequired: Option[String])~(oArrayFieldSettings: Option[ArrayFieldSettings]) =>
      ArrayField(None, oRequired.nonEmpty, oArrayFieldSettings, oElement)
    case "object"~(oRequired: Option[String])~(oObjectFieldSettings: Option[ObjectFieldSettings]) =>
      ObjectField(None, oRequired.nonEmpty, oObjectFieldSettings)
    case (fieldType: UnitType)~(oRequired: Option[String])~(oUnitFieldSettings: Option[UnitFieldSettings]) =>
      UnitField(None, fieldType, oRequired.nonEmpty, oUnitFieldSettings)
    case _ =>
      throw DocumentParsingException(s"array element is defined wrong")
  }

  def unitFieldSettings: Parser[UnitFieldSettings] = oBracket~opt(default)~opt(","~limits)~cBracket ^^ {
    case oBracket~oDefault~oLimits~cBracket if oBracket == cBracket =>
      val limits = getRightParser(oLimits) getOrElse List.empty[Expression]
      UnitFieldSettings(oDefault, limits)
    case _ =>
      throw DocumentParsingException(s"in unit field brackets don\'t match: $oBracket and $cBracket")
  }

  lazy val objectFieldSettings: PackratParser[ObjectFieldSettings] = oBracket~repsep(field, ",")~cBracket ^^ {
    case oBracket~properties~cBracket if oBracket == cBracket =>
      ObjectFieldSettings(properties)
    case _ =>
      throw DocumentParsingException(s"in object field brackets don\'t match: $oBracket and $cBracket")
  }

  def default: Parser[Expression] = "default:"~>expr
  def limits: Parser[List[Expression]] = "limits:"~>repsep(expr, ",")

  lazy val expr: PackratParser[Expression] = operand~opt(binOp~operand) ^^ {
    case left~Some(operator~right) =>
      BinaryOperation(left, operator, right)
    case left~None =>
      left
  }

  lazy val operand: PackratParser[Expression] =  value | predefinedConstant | "("~>expr<~")" | variable

  def predefinedConstant: Parser[PredefinedConstant] = username | length | self
  def username: Parser[PredefinedConstant] = "username" ^^ (_ => Username)
  def length: Parser[PredefinedConstant] = "length" ^^ (_ => Length)
  def self: Parser[PredefinedConstant] = ("self" | "_") ^^ (_ => Self)

  def variable: Parser[Variable] = rep(navigation)~repsep(fieldName, navigation) ^^ {
    case navigation~fields =>
      Variable(navigation ::: fields)
  }
  def navigation: Parser[String] = "."

  def value: Parser[Value] = intValue | floatValue | booleanValue | stringValue | arrayValue
  def arrayValue: Parser[Array] = "["~>repsep(value,",")<~"]" ^^ Array
  def intValue: Parser[IntUnitValue] = wholeNumber ^^ { num => IntUnitValue(num.toInt) }
  def floatValue: Parser[FloatUnitValue] = floatingPointNumber ^^ ( num => FloatUnitValue(num.toFloat))
  def booleanValue: Parser[BooleanUnitValue] = ("true" | "false") ^^ (b => BooleanUnitValue(b.toBoolean))
  def stringValue: Parser[StringUnitValue] = stringLiteral ^^ (s => StringUnitValue(trim(s)))

  def binOp: Parser[BinOp] = (
    plus | minus | multiply | divide | in | notIn | equal | notEqual | greater | less
      | greaterOrEqual | lessOrEqual | contains | notContains | logicBinOp
    )

  def plus: Parser[BinOp] = "+" ^^ (_ => Plus)
  def minus: Parser[BinOp] = "_" ^^ (_ => Minus)
  def multiply: Parser[BinOp] = "*" ^^ (_ => Multiply)
  def divide: Parser[BinOp] = "/" ^^ (_ => Divide)
  def in: Parser[BinOp] = "in" ^^ (_ => In)
  def notIn: Parser[BinOp] = "not in" ^^ (_ => NotIn)
  def equal: Parser[BinOp] = ("==" | "=") ^^ (_ => Equal)
  def notEqual: Parser[BinOp] = ("!=" | "<>") ^^ (_ => NonEqual)
  def greater: Parser[BinOp] = ">" ^^ (_ => Greater)
  def less: Parser[BinOp] = "<" ^^ (_ => Less)
  def greaterOrEqual: Parser[BinOp] = ">=" ^^ (_ => GreaterOrEqual)
  def lessOrEqual: Parser[BinOp] = "<=" ^^ (_ => LessOrEqual)
  def contains: Parser[BinOp] = "contains" ^^ (_ => Contains)
  def notContains: Parser[BinOp] = "not contains" ^^ (_ => NotContains)

  def logicBinOp: Parser[LogicBinOp]  = and | or
  def and: Parser[LogicBinOp] = "and" ^^ (_ => And)
  def or: Parser[LogicBinOp] = "or" ^^ (_ => Or)

  def required: Parser[String] = "required"
  def oBracket: Parser[BracketType] = "{" ^^ (_ => Curly) | "(" ^^ (_ => Round)
  def cBracket: Parser[BracketType] = "}" ^^ (_ => Curly) | ")" ^^ (_ => Round)
}

final case class DocumentContext(
  username: String,
  email: String,
  firstName: String,
  secondName: Option[String],
  lastName: String,
  additionalFields: Map[String, String] = Map.empty[String, String]
)

object DocumentTemplateProcessor {
  private object Parser extends DocumentParser {
    def parse(input: String): Template = parseAll(template, input) match {
      case Success(result, _) => result
      case Failure(msg, next) => throw DocumentParsingException(s"$msg $next")
    }
  }

  private def valueToJsValue(value: Value): JsValue = value match {
    case Empty => JsNull
    case IntUnitValue(v) => JsNumber(v)
    case FloatUnitValue(v) => JsNumber(v)
    case BooleanUnitValue(v) => JsBoolean(v)
    case StringUnitValue(v) => JsString(v)
    case Array(values) => JsArray(values map valueToJsValue)
  }

  private def jsValueToValue(value: JsValue): Value = value match {
    case JsBoolean(value) => BooleanUnitValue(value)
    case JsString(value) => StringUnitValue(value)
    case JsArray(value) => Array(value.map(jsValueToValue).toList)
    case JsNumber(value) =>
      try IntUnitValue(value.toInt)
      catch {
        case _: Exception => FloatUnitValue(value.toFloat)
      }
    case JsNull => Empty
  }

  def createEmptyDocument(template: String, context: DocumentContext): JsObject = {
    val parsedTemplate = Parser.parse(template)
    implicit val ctx: DocumentContext = context

    def createJsField(field: Field): Map[String, JsValue] = field match {
      case UnitField(name, _, _, oSettings) =>
        val default = oSettings match {
          case Some(UnitFieldSettings(oDefault, _)) => eval(oDefault getOrElse Empty)
          case None => Empty
        }
        Map(name.get -> valueToJsValue(default))
      case ObjectField(name, _, oSettings) =>
        val default = oSettings match {
          case Some(ObjectFieldSettings(properties)) =>
            Json.toJson(properties map createJsField reduce (_ ++ _))
          case None =>
            JsNull
        }
        Map(name.get -> default)
      case ArrayField(name, required, oSettings, _) =>
        val default = oSettings match {
          case Some(ArrayFieldSettings(oDefault, _)) =>
            oDefault match {
              case Some(defaultValue) => Json.toJson(valueToJsValue(eval(defaultValue)))
              case None => JsNull
            }
          case None => if (required) Json.arr() else JsNull
        }
        Map(name.get -> default)
    }

    val docFields = parsedTemplate.fields map createJsField reduce (_ ++ _)
    Json.toJson(docFields).as[JsObject]
  }

  def validateDocument(document: JsObject, template: String, context: DocumentContext): Seq[DocumentValidationError] = {
    val parsedTemplate = Parser.parse(template)
    implicit val ctx: DocumentContext = context

    def validateArrayValue(field: Field, name: String, value: JsArray): Seq[DocumentValidationError] = {
      implicit val self: JsValue = value

      field match {
        case UnitField(_, fieldType, required, oSettings) =>
          val values = value.value

          val typeErrors = fieldType match {
            case UnitType.Int =>
              values collect {
                case v if v.asOpt[Int].isEmpty => DocumentWrongFieldType(v.toString, "int")
              }
            case UnitType.String =>
              values collect {
                case v if v.asOpt[String].isEmpty => DocumentWrongFieldType(v.toString, "string")
              }
            case UnitType.Boolean =>
              values collect {
                case v if v.asOpt[Boolean].isEmpty => DocumentWrongFieldType(v.toString, "boolean")
              }
            case UnitType.Float =>
              values.collect {
                case v if v.asOpt[Float].isEmpty => DocumentWrongFieldType(v.toString, "float")
              }
          }

          val limitErrors = oSettings match {
            case Some(settings) =>
              settings.limits collect {
                case limit if !values.map(v => eval(limit)).forall(x => x) =>
                  DocumentFieldLimitNotMet(name, limit.toString)
              }
            case None => Nil
          }

          val requiredError =
            if (required && values.isEmpty) Seq(DocumentRequiredFieldNotFound("array field"))
            else Nil

          typeErrors.toSeq ++ limitErrors ++ requiredError

        case ObjectField(_, required, oObjectFields) =>
          val values = value.value

          val typeErrors = values.collect {
            case v if v.asOpt[JsObject].isEmpty => DocumentWrongFieldType(v.toString, "object")
          }.toSeq

          val fieldErrors = if (typeErrors.isEmpty) {
            oObjectFields match {
              case Some(objectFields) =>
                objectFields.properties flatMap {
                  field =>
                    values
                      .map(_.as[JsObject])
                      .map(_.value.toMap)
                      .flatMap(validateField(field, _))
                }
              case None => Nil
            }
          } else Nil
          val requiredError = {
            if (required && values.isEmpty) Seq(DocumentRequiredFieldNotFound("array field"))
            else Nil
          }
          typeErrors ++ fieldErrors ++ requiredError

        case ArrayField(_, required, oSettings, oElements) =>
          val values = value.value

          val typeError = values.collect {
            case v if v.asOpt[JsArray].isEmpty => DocumentWrongFieldType(v.toString, "array")
          }.toSeq

          val requiredError =
            if (required && values.isEmpty) Seq(DocumentRequiredFieldNotFound("array field"))
            else Nil

          val limitErrors = oSettings match {
            case Some(settings) =>
              settings.limits.collect {
                case limit if !eval(limit) =>
                  DocumentFieldLimitNotMet(s"array value $value", limit.toString)
              }
            case None => Nil
          }

          val elementsErrors = oElements match {
            case Some(elementDescription) =>
              values
                .map(_.as[JsArray])
                .flatMap(validateArrayValue(elementDescription, name + ".", _))
            case None => Nil
          }

          typeError ++ requiredError ++ limitErrors ++ elementsErrors
      }
    }

    def validateField(field: Field, jsFields: Map[String, JsValue]): Seq[DocumentValidationError] = field match {
      case UnitField(oName, fieldType, required, oSettings) =>
        val name = oName.get
        jsFields get name match {
          case Some(jsField) =>
            val typeChecked = fieldType match {
              case UnitType.Int => jsField.asOpt[Int].nonEmpty
              case UnitType.String => jsField.asOpt[String].nonEmpty
              case UnitType.Float => jsField.asOpt[Float].nonEmpty
              case UnitType.Boolean => jsField.asOpt[Boolean].nonEmpty
            }

            implicit val self: JsValue = jsField

            val typeError =
              if (typeChecked) Nil
              else Seq(DocumentWrongFieldType(name, fieldType.toString))

            val limitsErrors = oSettings match {
              case Some(settings) =>
                settings.limits collect {
                  case limit if !eval(limit) => DocumentFieldLimitNotMet(name, limit.toString)
                }
              case None => Nil
            }

            typeError ++ limitsErrors
          case None =>
            if (required) Seq(DocumentRequiredFieldNotFound(name))
            else Nil
        }
      case ObjectField(oName, required, oObjectFields) =>
        val name = oName.get

        jsFields get name match {
          case Some(jsField) =>
            val jsObject = jsField.as[JsObject]
            oObjectFields match {
              case Some(fields) => fields.properties.flatMap(validateField(_, jsObject.value.toMap))
              case None => Nil
            }
          case None =>
            if (required) Seq(DocumentRequiredFieldNotFound(name))
            else Nil
        }
      case ArrayField(oName, required, oSettings, oFields) =>
        val name = oName.get
        jsFields get name match {
          case Some(jsField) =>
            val jsArray = jsField.as[JsArray]
            implicit val self: JsValue = jsArray
            oSettings match {
              case Some(settings) =>
                val limitsErrors = settings.limits collect {
                  case limit if !eval(limit) => DocumentFieldLimitNotMet(name, limit.toString)
                }
                val elementsErrors = oFields match {
                  case Some(elements) => validateArrayValue(elements, name, jsArray)
                  case None => Nil
                }
                limitsErrors ++ elementsErrors
              case None => Nil
            }
          case None =>
            if (required) Seq(DocumentRequiredFieldNotFound(name))
            else Nil
        }
    }

    parsedTemplate.fields.flatMap(validateField(_, document.value.toMap))
  }

  private def eval(
    expression: Expression,
    document: JsObject = Json.obj(),
    field: Option[Field] = None
  )(implicit context: DocumentContext, self: JsValue = JsNull): Value = expression match {
    case v: Value => v
    case pc: PredefinedConstant =>
      pc match {
        case Length =>
          self match {
            case JsString(value) => IntUnitValue(value.length)
            case JsArray(value) => IntUnitValue(value.length)
            case _ => throw DocumentKeywordEvaluationException("length", self)
          }
        case Username => StringUnitValue(context.username)
        case Self =>
          self match {
            case JsString(value) => StringUnitValue(value)
            case JsNumber(value) => field match {
              case Some(f) =>
                f match {
                  case UnitField(_, fieldType, _, _) =>
                    fieldType match {
                      case UnitType.Int => IntUnitValue(value.toInt)
                      case UnitType.Float => FloatUnitValue(value.toFloat)
                      case _ =>
                        throw new Exception(s"could not cast self $self to number: unit field is of type $fieldType")
                    }
                  case _ => throw new Exception(s"could not cast self $self to number: field type is not unit ($f)")
                }
              case None =>
                self.asOpt[Int] match {
                  case Some(intValue) => IntUnitValue(intValue)
                  case None =>
                    self.asOpt[Float] match {
                      case Some(floatValue) => FloatUnitValue(floatValue)
                      case None =>
                        throw new Exception(s"could not cast $self to any number; field description is not provided")
                    }
                }
            }
          }
      }
    case Variable(path) =>
      jsValueToValue(path.foldLeft(document.as[JsValue])((j, segment) => (j \ segment).as[JsValue]))
    case BinaryOperation(left, operator, right) =>
      operator match {
        case Plus => eval(left) + eval(right)
        case Minus => eval(left) - eval(right)
        case Divide => eval(left) / eval(right)
        case Multiply => eval(left) * eval(right)
        case Equal => eval(left) == eval(right)
        case NonEqual => eval(left) != eval(right)
        case Less => eval(left) < eval(right)
        case LessOrEqual => eval(left) <= eval(right)
        case Greater => eval(left) > eval(right)
        case GreaterOrEqual => eval(left) >= eval(right)
        case Or => eval(left) or eval(right)
        case And => eval(left) and eval(right)
        case In => eval(left) in eval(right)
        case NotIn => eval(left) notIn eval(right)
        case Contains => eval(left) contains eval(right)
        case NotContains => eval(left) contains eval(right)
      }
  }
}