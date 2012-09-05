package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import java.nio.file._

object Reader extends Parsers {

  type Elem = Char

  def apply(input: String): Vect = Reader(new CharSequenceReader(input))

  def apply(input: Path): Vect = Reader(new CharSequenceReader(new String(Files.readAllBytes(input))))

  def apply(csr: scala.util.parsing.input.Reader[Char]): Vect = {
    val p = vrep((atomListParser(0)) <~ rep(eol))(csr)

    p match {
      case Success(res, next) if next.atEnd => res
      case f: Failure => sys.error(f.toString)
      case x => sys.error("Couldn't FULLY parse, result: " + x)
    }
  }

  private def atomListParser(depth: Int): Parser[Any] =
    rep(eol) ~>
      repN(depth, '\t') ~>
      vrep1sep(atomParser, rep(' ')) ~< rep(' ') ~
      vrep(eol ~> atomListParser(depth + 1)) ^^ (x => stitch(x._1, x._2))

  private def stitch(a: Vect, b: Vect) = {
    val t = (a concat b)

    if (t.length == 1)
      t.head
    else
      t
  }
      
  private def vrep(p: => Parser[Any]): Parser[Vect] = rep(p) ^^ (Vect(_))
  private def vrepsep(p: => Parser[Any], q: => Parser[Any]): Parser[Vect] = repsep(p, q) ^^ (Vect(_))
  private def vrep1sep(p: => Parser[Any], q: => Parser[Any]): Parser[Vect] = rep1sep(p, q) ^^ (Vect(_))

  private def atomParser = vectParser | intParser | quotedStringParser | symbolParser

  private def vectParser: Parser[Vect] = '(' ~> rep(' ') ~> vrepsep(atomParser, rep(' '))  ~< rep(' ') ~< ')'

  // TODO: allow arbitrary base
  private def intParser = rep1(digitParser) ^^ (x => numberListToNumber(x, base = 10))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (q => q.asDigit)

  // TODO: allow symbol escaping
  private def symbolParser = rep1(
    acceptIf(c =>
      !c.isWhitespace &&
        !c.isControl &&
        c != ')' &&
        c != '(' &&
        c != '\\' &&
        c != '"')(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ charListToSymbol

  private def quotedStringParser = '"' ~> rep(insideQuoteParser) ~< '"' ^^ charListToString

  // TODO: allow string escaping
  private def insideQuoteParser = acceptIf(c => c != '"' && c != '\n' && c != '\\')(c => "Unexpected '" + c + "' when parsing inside quote")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToString(letters: List[Char]) = new String(letters.toArray)
  private def charListToSymbol(letters: List[Char]) = Symbol(charListToString(letters))

  private def eol = elem('\n') // TODO: support windows, but give warnin'

  // get around annoying precedent rule of <~
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }


}
