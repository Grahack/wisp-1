package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object Reader extends Parsers {

  type Elem = Char

  def parse(input: String) = {
    val p = rep(atomListParser(0))(new CharSequenceReader(input))

    p match {
      case s: Success[List[Any]] => s.result
      case f: Failure => sys.error(f.toString)
    }
  }

  private def atomListParser(depth: Int): Parser[Any] =
    rep(eol) ~>
      repN(depth, '\t') ~>
      rep1sep(atomParser, rep1(' ')) ~
      rep(rep(' ') ~> eol ~> atomListParser(depth + 1)) ^^ (x => stitch(x._1, x._2))

  private def stitch(a: List[Any], b: List[Any]) = {
    val t = (a ++ b)

    if (t.length == 1)
      t.head
    else
      t
  }

  private def atomParser = intParser | quotedStringParser | symbolParser

  private def intParser = rep1(digitParser) ^^ (x => numberListToNumber(x, base = 10))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (q => q.asDigit)

  private def symbolParser = rep1(acceptIf(c => !c.isWhitespace && !c.isControl)(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ charListToSymbol

  // TODO: add string escaping
  private def quotedStringParser = '"' ~> rep(insideQuoteParser) ~< '"' ^^ charListToString
  private def insideQuoteParser = acceptIf(c => c != '"' && c != '\n')(c => "Unexpected '" + c + "' when parsing inside quote")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToString(letters: List[Char]) = new String(letters.toArray)
  private def charListToSymbol(letters: List[Char]) = Symbol(charListToString(letters))

  private def eol = elem('\n') // TODO: support windows, but give warnin'

  // get around annoying precedent rule of <~
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

}
