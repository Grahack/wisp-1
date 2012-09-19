package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import java.nio.file._

object Reader extends Parsers {

  type Elem = Char

  def apply(path: Path): (Set[Path], Any) = {
    


    val csr = new CharSequenceReader(new String(Files.readAllBytes(path)))

    val p = rep((atomListParser(0)) <~ rep(eol))(csr)

    val forms = p match {
      case Success(res, next) => {
        if (next.atEnd)
          res
        else
          sys.error("Couldn't FULLY parse. We parsed: " + res + " but didn't get: " + next)
      }
      case f: Failure => sys.error("Parser failure: " + f.toString)
    }

    forms.headOption match {
      case Some('import +: paths) =>
        require(forms.length == 2, "A top level with an import, must only have two forms")

        val imports = paths.data.map(x => path.resolveSibling(x.asInstanceOf[String]))

        (imports.self.toList.toSet -> forms.last)

      case _ =>
        require(forms.length == 1, "A top level with an import, must only have a single form")
        (Set() -> forms.head)
    }
  }

  private def atomListParser(depth: Int): Parser[Any] =
    rep(eol) ~>
      repN(depth, '\t') ~>
      rep1sep(atomParser, ' ') ~
      rep(eol ~> atomListParser(depth + 1)) ^^ (x => stitch(x._1, x._2))

  private def stitch(a: List[Any], b: List[Any]) = {
    val t = (a ++ b)

    if (t.length == 1)
      t.head
    else
      Call(t.head, Vect.fromSeq(t.tail))
  }

  private def atomParser: Parser[Any] = (vectParser | intParser | quotedStringParser | symbolParser) ~
    opt('(' ~> repsep(atomParser, ' ') ~< ')') ^^ { x => if (x._2.isEmpty) x._1 else Call(x._1, Vect.fromSeq(x._2.get)) }

  private def vectParser: Parser[Vect] = '[' ~> repsep(atomParser, ' ') ~< ']' ^^ (x => Vect.fromSeq(x))

  // TODO: allow arbitrary base
  private def intParser = rep1(digitParser) ^^ (x => numberListToNumber(x, base = 10))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (q => q.asDigit)

  private def symbolParser = rep1(
    acceptIf(c =>
      !c.isWhitespace && !c.isControl
        && c != '(' && c != ')'
        && c != '[' && c != ']'
        && c != ';' && c != '"')(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ charListToSymbol

  private def quotedStringParser = '"' ~> rep(insideQuoteParser) ~< '"' ^^ charListToVect

  // TODO: allow string escaping
  private def insideQuoteParser = acceptIf(c => c != '"' && c != '\n' && c != '\\')(c => "Unexpected '" + c + "' when parsing inside quote")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToVect(letters: List[Char]) = Vect.fromSeq(letters)
  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private def eol = elem('\n') // TODO: support windows, but give warnin'

  // get around annoying precedent rule of <~
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }
}
