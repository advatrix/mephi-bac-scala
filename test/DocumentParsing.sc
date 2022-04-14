// import controllers.DocumentFieldDescription

import play.api.libs.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsPath, JsString, JsValue, Json, __}

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
  case object Plus extends BinOp
  case object Minus extends BinOp
  case object Multiply extends BinOp
  case object Divide extends BinOp
  case object Equal extends BinOp
  case object NonEqual extends BinOp
  case object Greater extends BinOp
  case object Less extends BinOp
  case object GreaterOrEqual extends BinOp
  case object LessOrEqual extends BinOp
  case object In extends BinOp
  case object NotIn extends BinOp
  case object Contains extends BinOp
  case object NotContains extends BinOp
  sealed trait LogicBinOp extends BinOp
  case object And extends LogicBinOp
  case object Or extends LogicBinOp

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

  sealed trait Expression
  case class BinaryOperation(left: Expression, operator: BinOp, right: Expression) extends Expression
  case class Variable(path: List[String]) extends Expression   // refers to document field value
  sealed abstract class Value(val value: Any) extends Expression {
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
      case t: IntUnitValue => FloatUnitValue(this.value + t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value + t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value + (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }

    override def *(that: Value): FloatUnitValue = that match {
      case t: IntUnitValue => FloatUnitValue(this.value + t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value + t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value + (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
    }

    override def /(that: Value): FloatUnitValue = that match {
      case t: IntUnitValue => FloatUnitValue(this.value + t.value)
      case t: FloatUnitValue => FloatUnitValue(this.value + t.value)
      case t: BooleanUnitValue => FloatUnitValue(this.value + (if (t.value) 1F else 0F))
      case _ => throw DocumentExpressionEvaluationException("+", this, that)
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
    override def +(that: Value): Value = that
    override def -(that: Value): Value = that
    override def *(that: Value): Value = throw DocumentExpressionEvaluationException("*", this, that)
    override def /(that: Value): Value = throw DocumentExpressionEvaluationException("/", this, that)
    override def <(that: Value): BooleanUnitValue = throw DocumentExpressionEvaluationException("order", this, that)
    override def in(that: Value): BooleanUnitValue = BooleanUnitValue(true)
  }

  sealed trait PredefinedConstant extends Expression
  case object Username extends PredefinedConstant
  case object Length extends PredefinedConstant
  case object Self extends PredefinedConstant

  sealed trait FieldSettings
  case class UnitFieldSettings(default: Option[Expression], limits: List[Expression])
  case class ArrayFieldSettings(elements: List[Field], limits: List[Expression])
  case class ObjectFieldSettings(properties: List[Field])

  sealed trait Field
  case class UnitField(name: String, fieldType: UnitType.UnitType, required: Boolean, settings: Option[UnitFieldSettings]) extends Field
  case class ArrayField(name: String, required: Boolean, settings: Option[ArrayFieldSettings]) extends Field
  case class ObjectField(name: String, required: Boolean, settings: Option[ObjectFieldSettings]) extends Field

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
        case "array"~(oRequired: Option[String])~(oArrayFieldSettings: Option[ArrayFieldSettings]) =>
          ArrayField(name, oRequired.nonEmpty, oArrayFieldSettings)
        case "object"~(oRequired: Option[String])~(oObjectFieldSettings: Option[ObjectFieldSettings]) =>
          ObjectField(name, oRequired.nonEmpty, oObjectFieldSettings)
        case (fieldType: UnitType)~(oRequired: Option[String])~(oUnitFieldSettings: Option[UnitFieldSettings]) =>
          UnitField(name, fieldType, oRequired.nonEmpty, oUnitFieldSettings)
        case _ =>
          throw DocumentParsingException(s"$name field is defined wrong")
      }
  }

  def fieldName: Parser[String] = "\""~>"[a-zA-Z0-9а-яА-Я_]+".r<~"\""

  def arrayFieldDefinition: Parser[String ~ Option[String] ~ Option[ArrayFieldSettings]] =
    "array"~opt(Required)~opt(arrayFieldSettings)

  def objectFieldDefinition: Parser[String ~ Option[String] ~ Option[ObjectFieldSettings]] =
    "object"~opt(Required)~opt(objectFieldSettings)

  def unitFieldDefinition: Parser[UnitType ~ Option[String] ~ Option[UnitFieldSettings]] =
    unitType~opt(Required)~opt(unitFieldSettings)

  def unitType: Parser[UnitType] = intUnitType | floatUnitType | booleanUnitType | stringUnitType

  def intUnitType: Parser[UnitType] = "int" ^^ (_ => UnitType.Int)
  def floatUnitType: Parser[UnitType] = "float" ^^ (_ => UnitType.Float)
  def booleanUnitType: Parser[UnitType] = "boolean" ^^ (_ => UnitType.Boolean)
  def stringUnitType: Parser[UnitType] = "string" ^^ (_ => UnitType.String)

  lazy val arrayFieldSettings: PackratParser[ArrayFieldSettings] = oBracket~repsep(field, ",")~opt(","~limits)~cBracket ^^ {
    case oBracket~elements~oLimits~cBracket if oBracket == cBracket =>
      val limits = getRightParser(oLimits) getOrElse List.empty[Expression]
      ArrayFieldSettings(elements, limits)
    case _ =>
      throw DocumentParsingException(s"in array field brackets don\'t match: $oBracket and $cBracket")
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

object DocumentParsing extends DocumentParser {
  def analyze(input: String) =
    parseAll(template, input)
}

val d = new DocumentParser
// d.parseAll(d.operand, "2 not in [1, 2, 3]")
// d.parseAll(d.operand, "2 not in [1, 2, 3]")

d.parseAll(d.value, "-17")
d.parseAll(d.value, "\"string\"")
d.parseAll(d.booleanValue, "true")
d.parseAll(d.floatValue, "5.4")

d.parseAll(d.operand, "(2 not in [1, 2, 3])")
d.parseAll(d.operand, "(2 not in [1, 2, 3])")
d.parseAll(d.operand, "true")
d.parseAll(d.operand, "(true or false)")
// d.parseAll(d.operand, "true or false")
d.parseAll(d.operand, "(2)")
d.parseAll(d.operand, "(2 + 2)")

d.parseAll(d.expr, "2 not in [1, 2, 3]")
d.parseAll(d.expr, "2 not in [1, 2, 3]")
d.parseAll(d.expr, "(2 not in [1, 2, 3])")
d.parseAll(d.expr, "(2 not in [1, 2, 3])")
d.parseAll(d.expr, "true")
d.parseAll(d.expr, "(true or false)")
d.parseAll(d.expr, "true or false")
d.parseAll(d.expr, "(2)")
d.parseAll(d.expr, "(2 + 2)")

d.parseAll(d.expr, "2 + (2 + 2)")
d.parseAll(d.expr, "2 + (2 not in [1, 2, 3])")
d.parseAll(d.expr, "(4 in [1, 2, 3]) or (2 not in [1, 2, 3])")
d.parseAll(d.expr, "4 < 5")
d.parseAll(d.expr, "4 = 5")
d.parseAll(d.expr, "4 == 5")
d.parseAll(d.expr, "(4 == 5) or (2 not in [1, 2, 3])")
d.parseAll(d.expr, "(..\"string\".\"field\" = 13) or (2 not in [1, 2, 3])")

d.parseAll(d.expr, "length < 15")

d.parseAll(d.default, "default: length < 15")
d.parseAll(d.default, "default: 5")
d.parseAll(d.default, "default: 4 + 6")

d.parseAll(d.limits, "limits: length < 14")
d.parseAll(d.limits, "limits: (length < 14) and (length > 45)")
d.parseAll(d.limits, "limits: true and false")
d.parseAll(d.limits, "limits: 1, 2, 3")
d.parseAll(d.limits, "limits: [1, 2, 3]")
d.parseAll(d.limits, "limits: length < 15, _ > 4")

d.parseAll(d.unitFieldSettings, "{ default: length < 15, limits: 1, 2, 3 }")
d.parseAll(d.unitFieldSettings, "{ default: ..\"field\".\"string\", limits: true, (2 in [1, 2, 3]), 3 }")

d.parseAll(d.unitType, "int")
d.parseAll(d.unitType, "float")
d.parseAll(d.unitType, "string")
d.parseAll(d.unitType, "boolean")

d.parseAll(d.unitFieldDefinition, "int required { default: ..\"field\".\"string\", limits: true, (2 in [1, 2, 3]), 3 }")

d.parseAll(d.fieldName, "\"qwerty\"")
d.parseAll(d.fieldName, "\"qwerty_qwerty\"")
d.parseAll(d.fieldName, "\"qwertyQwerty\"")
d.parseAll(d.fieldName, "\"qwertyЙЦУкен__\"")

val simpleIntField =  "\"fieldName\" int required { default: ..\"field\".\"string\", limits: true, (2 in [1, 2, 3]), 3 }"

d.parseAll(d.field, simpleIntField)

d.parseAll(d.template, s"document \"DocumentName\" { $simpleIntField }")

val simpleObjectFieldSettings = s"{ $simpleIntField }"
d.parseAll(d.objectFieldSettings, simpleObjectFieldSettings)

val simpleObjectFieldDefinition = s"object { $simpleIntField }"
d.parseAll(d.objectFieldDefinition, simpleObjectFieldDefinition)

val simpleObjectField = s"\"simple_object_field\" $simpleObjectFieldDefinition"
d.parseAll(d.field, simpleObjectField)

val document1 = s"document \"int_object\" { $simpleIntField, $simpleObjectField }"
d.parseAll(d.template, document1)

val complexObjectFieldDefinition = s"object { $simpleIntField, $simpleObjectField }"
d.parseAll(d.objectFieldDefinition, complexObjectFieldDefinition)

val complexObjectField = s"\"complex_object_field\" $complexObjectFieldDefinition"
d.parseAll(d.field, complexObjectField)

val document2 = s"document \"document_int_complex_object\" { $simpleIntField, $complexObjectField }"
d.parseAll(d.template, document2)

val simpleArrayFieldSettings = s"{ $simpleIntField, limits: length < 15 }"
d.parseAll(d.arrayFieldSettings, simpleArrayFieldSettings)

val simpleArrayFieldDefinition = s"array $simpleArrayFieldSettings"
d.parseAll(d.arrayFieldDefinition, simpleArrayFieldDefinition)

val simpleArrayField = s"\"array\" $simpleArrayFieldDefinition"
d.parseAll(d.field, simpleArrayField)

val document3 = s"document \"document_int_obj_arr\" { $simpleArrayField, $complexObjectField, $simpleIntField }"
d.parseAll(d.template, document3)

final case class DocumentContext(
  username: String,
  email: String,
  firstName: String,
  secondName: Option[String],
  lastName: String,
  additionalFields: Map[String, String] = Map.empty[String, String]
)

object DocumentTemplateInterpreter {
  // import UnitType._

  private object Parser extends DocumentParser {
    def parseExpr(input: String): Expression = parseAll(expr, input) match {
      case Success(result, _) => result
      case Failure(msg, next) => throw new DocumentParsingException(s"$msg $next")
    }
    def parse(input: String): Template = parseAll(template, input) match {
      case Success(result, _) => result
      case Failure(msg, next) => throw new DocumentParsingException(s"$msg $next")
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
      case UnitField(name, fieldType, required, oSettings) =>
        val default = oSettings match {
          case Some(UnitFieldSettings(oDefault, _)) => eval(oDefault getOrElse Empty)
          case None => Empty
        }
        Map(name -> valueToJsValue(default))

      case ObjectField(name, required, oSettings) =>
        val default = oSettings match {
          case Some(ObjectFieldSettings(properties)) =>
            Json.toJson(properties map createJsField reduce (_ ++ _))
          case None =>
            JsNull
        }
        Map(name -> default)

      case ArrayField(name, required, oSettings) =>
        val default = oSettings match {
          case Some(ArrayFieldSettings(elements, _)) =>
            Json.arr(elements map createJsField)
          case None => if (required) Json.arr() else JsNull
        }
        Map(name -> default)
    }

    val docFields = parsedTemplate.fields map createJsField reduce (_ ++ _)

    Json.toJson(docFields).as[JsObject]
  }

  def evalExternal(input: String): Value = {
    implicit val defaultDocumentContext: DocumentContext = DocumentContext(
      "username", "email", "firstName", None, "lastName"
    )
    eval(Parser.parseExpr(input))
  }

  // TODO: make it private
  private def eval(expression: Expression)(
    implicit context: DocumentContext, document: JsObject = Json.obj(), self: JsValue = JsNull, field: Option[Field] = None
  ): Value = expression match {
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
            case JsNumber(value) => field.asInstanceOf[UnitField].fieldType match {
              case UnitType.Int => IntUnitValue(value.toInt)
              case UnitType.Float => FloatUnitValue(value.toFloat)
              case _ => throw new Exception(s"could not cast self $self to number")
            }
          }
      }
    case Variable(path) =>
      jsValueToValue(
        path.foldLeft(document.as[JsValue])((j, segment) => (j \ segment).as[JsValue])
      )
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

val defaultDocumentContext = DocumentContext(
  "username", "email", "firstName", None, "lastName"
)

val expression = d.parseAll(d.expr, "1 + 1")
val expression = d.parseAll(d.expr, "4 in [1, 2, 3, 4]")

DocumentTemplateInterpreter.evalExternal("1 + 1")
DocumentTemplateInterpreter.evalExternal("(1 + 1) + 1")
DocumentTemplateInterpreter.evalExternal("true or false")
DocumentTemplateInterpreter.evalExternal("(2 + 3) < 11")
DocumentTemplateInterpreter.evalExternal("username != \"not username\"")
print(DocumentTemplateInterpreter.evalExternal("4 in [1, 2, 3, 4]"))


val templateWithDefaults = "document \"defaults\" { \"int_field\" int { default: 4 } }"
d.parseAll(d.template, templateWithDefaults)

val defaultDocumentContext: DocumentContext = DocumentContext(
  "username", "email", "firstName", None, "lastName"
)
DocumentTemplateInterpreter.createEmptyDocument(templateWithDefaults, defaultDocumentContext)



