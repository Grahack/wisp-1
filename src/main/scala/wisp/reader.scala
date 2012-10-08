package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import java.nio.file._

object Reader extends Parsers {

  import scala.util.parsing.input.Positional

  type Elem = Char

  def apply(path: Path): (Seq[String], Any) = apply(new String(Files.readAllBytes(path)))

  def apply(contents: String): (Seq[String], Any) = {

    val csr = new scala.util.parsing.input.CharArrayReader(contents.toCharArray())

    val p = fileParser(csr)

    val forms = p match {
      case Success(res, next) => {
        if (next.atEnd)
          res
        else
          sys.error("Couldn't FULLY parse. We parsed: " + res + " but didn't get: " + next)
      }
      case f: Failure => sys.error("Parser failure: " + f.toString)
      case e: Error => sys.error("Parser error: " + e.toString)
    }

    forms.value.headOption match {
      case Some('import #:: paths) =>
        require(forms.value.length == 2, "A file with an import, must only have two forms")

        val imports = paths.map(_.asInstanceOf[String])

        (imports -> forms.value.last)

      case _ =>
        require(forms.value.length == 1, "A file without an import, must only have a single form. Found: " + forms)
        (Seq() -> forms.value.head)
    }
  }
  
  private def fileParser: Parser[WList] = 
    positioned(rep(WListParser(0)) ~< rep(eol) ^^ (x => new WList(x.toStream) with Positional))

  private def WListParser(depth: Int): Parser[W] =
    rep(blankLine) ~>
      repN(depth, '\t') ~>
      rep1sep(WParser, singleSpace) ~
      rep(WListParser(depth + 1)) ^^ (x => stitch(x._1, x._2))

  private def stitch(a: Seq[W], b: Seq[W]) = {
    assert(a.length >= 1)
    if (a.length == 1 && b.length == 0)
      a.head
    else
      new WList(a.toStream ++ b.toStream)
  }

  private def comment = ';' ~> rep(acceptIf(_ != '\n')("Didn't expect: " + _ + " in comment"))
  private def blankLine = rep(elem(' ') | elem('\t')) ~> opt(comment) ~< eol

  private def WParser: Parser[W with Positional] =
    positioned((numParser | charParser | listParser | literalStringParser | literalListParser | symbolParser) ~ opt('.' ~> WParser) ^^
      (x => if (x._2.isDefined) new WList(Stream(x._1, x._2.get)) with Positional else x._1))

  private def charParser =
    positioned('~' ~> acceptIf(!special(_))("expected char, but found: " + _) ^^ (x => new WChar(x) with Positional))

  private def listParser =
    positioned('(' ~> repsep(WParser, singleSpace) ~< ')' ^^ (x => new WList(x.toStream) with Positional))

  private def literalListParser =
    positioned('[' ~> repsep(WParser, singleSpace) ~< ']' ^^ (x => new WList(ListMake #:: (x.toStream: Stream[W])) with Positional))

  // TODO: allow arbitrary base
  private def numParser = positioned(rep1(digitParser) ^^ (x => new WNum(numberListToNumber(x, base = 10)) with Positional))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (_.asDigit)

  private def symbolParser = positioned(rep1(
    acceptIf(!special(_))(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ (x => new WSym(charListToSymbol(x)) with Positional))

  private def special(c: Char) =
    c.isWhitespace || c.isControl ||
      c == '(' || c == ')' ||
      c == '[' || c == ']' ||
      c == '~' || c == '"' ||
      c == ';' || c == '.'

  private def literalStringParser = '"' ~> rep(insideLiteralParser) ~< '"' ^^ (x => new WList(charListToStream(x)) with Positional)

  // TODO: allow string escaping
  private def insideLiteralParser = acceptIf(x => x != '"' && x != '\n')("Unexpected '" + _ + "' when parsing inside a literal string")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToStream(letters: List[Char]) = ListMake #:: (letters.map(new WChar(_)).toStream: Stream[W])
  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private def eol = elem('\n') // warn if retarded-end line found?

  private def singleSpace = elem(' ')

  // get around annoying precedent rule of <~
  import scala.language.implicitConversions
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

  def startingEnv: Map[Symbol, W] = null
}
