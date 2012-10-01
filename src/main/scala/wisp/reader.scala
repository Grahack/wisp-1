package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import java.nio.file._

object Reader extends Parsers {

  type Elem = Char

  def apply(path: Path): (Seq[String], Any) = apply(new String(Files.readAllBytes(path)))

  def apply(contents: String): (Seq[String], Any) = {

    val csr = new CharSequenceReader(contents)

    val p = (rep((atomListParser(0)) <~ rep(eol)) ~< eof)(csr)

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
        require(forms.length == 2, "A file with an import, must only have two forms")

        val imports = paths.data.map(_.asInstanceOf[String])

        (imports -> forms.last)

      case _ =>
        require(forms.length == 1, "A file without an import, must only have a single form. Found: " + forms)
        (Seq() -> forms.head)
    }
  }

  private def eof = acceptIf(_.toInt == 26)(c => "Expected eof, but found: '" + c + "' (" + c.toInt + ") instead") // wtf is 26?

  private def atomListParser(depth: Int): Parser[Any] =
    rep(blankLine) ~>
      repN(depth, '\t') ~>
      rep1sep(atomParser, singleSpace) ~ 
      rep(atomListParser(depth + 1)) ^^ (x => stitch(x._1, x._2))

  private def stitch(a: Seq[Any], b: Seq[Any]) = {
    assert(a.length >= 1)
    if (a.length == 1 && b.length == 0)
      a.head
    else
      Vect.fromSeq(a ++ b)
  }
      
  private def comment = ';' ~> rep(acceptIf(_ != '\n')("Didn't expect: " + _ + " in comment"))
  private def blankLine = rep(elem(' ') | elem('\t')) ~> opt(comment) ~< eol

  private def atomParser: Parser[Any] =
    intParser | charParser | vectParser | literalStringParser | symbolParser

  private def charParser: Parser[Char] = '\'' ~> acceptIf(!special(_))(_ => "expected char")
  private def vectParser: Parser[Vect] = '(' ~> repsep(atomParser, singleSpace) ~< ')' ^^ (Vect.fromSeq(_))

  private def singleSpace = elem(' ')

  // TODO: allow arbitrary base
  private def intParser = rep1(digitParser) ^^ (numberListToNumber(_, base = 10))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (_.asDigit)

  private def symbolParser = rep1(
    acceptIf(!special(_))(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ charListToSymbol

  private def special(c: Char) =
    c.isWhitespace || c.isControl ||
      c == '(' || c == ')' ||
      c == ''' || c == '"' ||
      c == ';'

  private def literalStringParser = '"' ~> rep(insideLiteralParser) ~< '"' ^^ charListToVect

  // TODO: allow string escaping
  private def insideLiteralParser = acceptIf(x => x != '"' && x != '\n')("Unexpected '" + _ + "' when parsing inside a literal string")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToVect(letters: List[Char]) = WFunc.VectMake +: Vect.fromSeq(letters)
  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private def eol = elem('\n') // TODO: support windows, but give warnin'

  // get around annoying precedent rule of <~
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }
}
