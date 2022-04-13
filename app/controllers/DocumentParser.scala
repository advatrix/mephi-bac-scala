package controllers

import scala.util.parsing.combinator._

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}



sealed trait DocumentFieldType
case object DFT_Int extends DocumentFieldType
case object DFT_Float extends DocumentFieldType
case object DFT_Boolean extends DocumentFieldType
case object DFT_String extends DocumentFieldType
case object DFT_Object extends DocumentFieldType
case object DFT_Array extends DocumentFieldType

case class DocumentFieldDescription(name: String, fieldType: DocumentFieldType)
case class DocumentTemplate(name: String, fields: List[DocumentFieldDescription])

class DocumentParser extends JavaTokenParsers {

  def template: Parser[DocumentTemplate] = "document"~>stringLiteral~"{"~rep(fieldDescription)<~"}" ^^ {
    case documentName~_~fieldDescriptions =>
      DocumentTemplate(documentName, fieldDescriptions)
  }

  def fieldDescription: Parser[DocumentFieldDescription] =
}
