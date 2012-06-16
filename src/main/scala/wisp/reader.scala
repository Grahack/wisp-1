package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object Reader extends Parsers {

  type Elem = Char

  def apply(input: String) = {
    val p = rep(atomListParser(0))(new CharSequenceReader(input))

    p match {
      case s: Success[List[Any]] => expand(s.result).asInstanceOf[List[Any]]
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

  private def atomParser = listParser | intParser | quotedStringParser | symbolParser

  private def listParser: Parser[List[Any]] = '(' ~> repsep(atomParser, rep1(' ')) ~< ')'

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

 
  
  val builtIns: Map[Symbol, Any] = Interpretter.builtinFunctions.map( x => (x.name, x) ).toMap +
   (Symbol("#true") -> true) +
   (Symbol("#false") -> false) +
   (Symbol("#author") -> "Eric Springer")

  private def expand(v: Any): Any = v match {
    case l: List[_] => l.map(expand(_))
    case s: Symbol => builtIns.getOrElse(s, s)
    case z: String => z
    case i: Int => i
    case _ => sys.error("Unknown type of: " + v)
  }

}
