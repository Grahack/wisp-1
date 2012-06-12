package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object Reader extends Parsers {

  type Elem = Char

  def parse(input: String) = {
    val p = rep(atomListParser(0))(new CharSequenceReader(input))

    println("Debug: read in: " + p)

    p match {
      case s: Success[List[Any]] => s.result
      case f: Failure => sys.error(f.toString)
    }
  }

  private def atomListParser(depth: Int): Parser[Any] =
    rep(eol) ~>
      repN(depth, '\t') ~>
      rep1sep(atomParser, rep1(' ')) ~
      rep(eol ~> atomListParser(depth + 1)) ^^ (x => {
        val t = (x._1 ++ x._2)

        if (t.length == 1)
          t.head
        else
          t
      })

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
