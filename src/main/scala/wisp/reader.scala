package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object Reader extends Parsers {

  type Elem = Char

  def parse(input: String) = {
    rep(atomListParser(0))(new CharSequenceReader(input))
  }

  private def atomListParser(depth: Int): Parser[List[Any]] =
    rep(eol) ~>
      repN(depth, '\t') ~>
      rep1sep(atomParser, rep1(' ')) ~
      rep(eol ~> atomListParser(depth + 1)) ^^ (x => (x._1 ++ x._2))

  private def atomParser = intParser | quotedStringParser | symbolParser

  private def intParser = rep1(digitParser) ^^ (x => numberListToNumber(x, base = 10))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (q => q.asDigit)

  private def symbolParser = rep1(acceptIf(c => !c.isWhitespace && !c.isControl)(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ charListToSymbol

  // TODO: add string escaping
  private def quotedStringParser = '"' ~> rep(acceptIf(c => c != '"' && c != '\n')(c => "Unexpected '" + c + "' when parsing inside quote")) <~ '"' ^^ charListToString

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToString(letters: List[Char]) = new String(letters.toArray)
  private def charListToSymbol(letters: List[Char]) = Symbol(charListToString(letters))

  private def eol = elem('\n') // TODO: support windows, but give warnin'

}
