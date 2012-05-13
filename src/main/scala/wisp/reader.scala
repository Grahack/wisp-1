package wisp

import scala.util.parsing.combinator._

object Reader extends JavaTokenParsers {
  def parse(line: String) = parseAll(program, line) match {
    case Success(r, _) => r
  }

  def program: Parser[List[Any]] = rep(exp)
  def list: Parser[List[Any]] = "(" ~> rep(exp) <~ ")"
  def exp: Parser[Any] = (
    integer
    | stringLiteral
    | quote
    | symbol
    | list)

  def integer: Parser[Int] = wholeNumber ^^ (n => n.toInt)
  def symbol: Parser[Symbol] = """[^() ]+""".r ^^ (x => Symbol(x))
  def quote = "'" ~> exp ^^ (e => List("quote", e))
}