// import controllers.DocumentFieldDescription

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
  // import UnitType.UnitTy

  case class DocumentParsingException(msg: String) extends Exception {
    override def toString: String = msg
  }


  sealed trait Expression
  case class BinaryOperation(left: Expression, operator: BinOp, right: Expression) extends Expression
  case class Variable(path: List[String]) extends Expression   // refers to document field value
  sealed abstract class Value(val value: Any) extends Expression {
    def ==(that: Value): Boolean = this.value == that.value
  }
  final case class IntUnitValue(override val value: Int) extends Value
  final case class FloatUnitValue(override val value: Float) extends Value
  final case class BooleanUnitValue(override val value: Boolean) extends Value
  final case class StringUnitValue(override val value: String) extends Value
  final case class Array(override val value: List[Value]) extends Value

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
  case class ArrayField(name: String, settings: Option[ArrayFieldSettings]) extends Field
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
        case "array"~(oArrayFieldSettings: Option[ArrayFieldSettings]) =>
          ArrayField(name, oArrayFieldSettings)
        case "object"~(oRequired: Option[String])~(oObjectFieldSettings: Option[ObjectFieldSettings]) =>
          ObjectField(name, oRequired.nonEmpty, oObjectFieldSettings)
        case (fieldType: UnitType)~(oRequired: Option[String])~(oUnitFieldSettings: Option[UnitFieldSettings]) =>
          UnitField(name, fieldType, oRequired.nonEmpty, oUnitFieldSettings)
        case _ =>
          throw DocumentParsingException(s"$name field is defined wrong")
      }
  }

  def fieldName: Parser[String] = "\""~>"[a-zA-Z0-9а-яА-Я_]+".r<~"\""

  def arrayFieldDefinition: Parser[String ~ Option[ArrayFieldSettings]] = "array"~opt(arrayFieldSettings)

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

  // def expr: Parser[Expression] = bracketExpr | simpleExpr | exprLogicBinOpExpr

//  lazy val expr2: PackratParser[Expression] = term~opt(binOp~term) ^^ {
//    case l^^Some(op~r) => BinaryOperation(l, op, r)
//    case l^^None => l
//  }

//  lazy val term: PackratParser[Expression] = simpleExpr | "("~>expr2<~")"


//  lazy val expr: PackratParser[Expression] = bracketExpr | simpleExpr | exprBinOpExpr

//  lazy val bracketExpr: PackratParser[Expression] = "("~>expr<~")"

//  lazy val exprBinOpExpr: PackratParser[Expression] = expr~binOp~expr ^^ {
//    case left~operator~right =>
//      BinaryOperation(left, operator, right)
//  }

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
      Variable(navigation ::: fields.map(trim))
  }
  def navigation: Parser[String] = "."

  def value: Parser[Value] = intValue | floatValue | booleanValue | stringValue | arrayValue
  def arrayValue: Parser[Array] = "["~>repsep(value,",")<~"]" ^^ (v => Array(v))
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
