import scala.util.parsing.combinator._

class Parser extends JavaTokenParsers {
  def a: Parser[String] = stringLiteral ^^ {
    x => s"$x is parsed"
  }

}

object Parsing extends Parser {
  def analyze(input: String) =
    parseAll(a, input)
}

val input = """  "a a"   """
print(Parsing.analyze(input))

