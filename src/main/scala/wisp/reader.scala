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
  
  private def fileParser: Parser[AtomList] = 
    positioned(rep(atomListParser(0)) ~< rep(eol) ^^ (x => new AtomList(x.toStream) with Positional))

  private def atomListParser(depth: Int): Parser[Atom] =
    rep(blankLine) ~>
      repN(depth, '\t') ~>
      rep1sep(atomParser, singleSpace) ~
      rep(atomListParser(depth + 1)) ^^ (x => stitch(x._1, x._2))

  private def stitch(a: Seq[Atom], b: Seq[Atom]) = {
    assert(a.length >= 1)
    if (a.length == 1 && b.length == 0)
      a.head
    else
      new AtomList(a.toStream ++ b.toStream)
  }

  private def comment = ';' ~> rep(acceptIf(_ != '\n')("Didn't expect: " + _ + " in comment"))
  private def blankLine = rep(elem(' ') | elem('\t')) ~> opt(comment) ~< eol

  private def atomParser: Parser[Atom with Positional] =
    positioned((numParser | charParser | listParser | literalStringParser | literalListParser | symbolParser) ~ opt('.' ~> atomParser) ^^
      (x => if (x._2.isDefined) new AtomList(Stream(x._1, x._2.get)) with Positional else x._1))

  private def charParser =
    positioned('~' ~> acceptIf(!special(_))("expected char, but found: " + _) ^^ (x => new AtomChar(x) with Positional))

  private def listParser =
    positioned('(' ~> repsep(atomParser, singleSpace) ~< ')' ^^ (x => new AtomList(x.toStream) with Positional))

  private def literalListParser =
    positioned('[' ~> repsep(atomParser, singleSpace) ~< ']' ^^ (x => new AtomList(ListMake #:: (x.toStream: Stream[Atom])) with Positional))

  // TODO: allow arbitrary base
  private def numParser = positioned(rep1(digitParser) ^^ (x => new AtomNum(numberListToNumber(x, base = 10)) with Positional))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (_.asDigit)

  private def symbolParser = positioned(rep1(
    acceptIf(!special(_))(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ (x => new AtomSym(charListToSymbol(x)) with Positional))

  private def special(c: Char) =
    c.isWhitespace || c.isControl ||
      c == '(' || c == ')' ||
      c == '[' || c == ']' ||
      c == '~' || c == '"' ||
      c == ';' || c == '.'

  private def literalStringParser = '"' ~> rep(insideLiteralParser) ~< '"' ^^ (x => new AtomList(charListToStream(x)) with Positional)

  // TODO: allow string escaping
  private def insideLiteralParser = acceptIf(x => x != '"' && x != '\n')("Unexpected '" + _ + "' when parsing inside a literal string")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToStream(letters: List[Char]) = ListMake #:: (letters.map(new AtomChar(_)).toStream: Stream[Atom])
  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private def eol = elem('\n') // warn if retarded-end line found?

  private def singleSpace = elem(' ')

  // get around annoying precedent rule of <~
  import scala.language.implicitConversions
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

  import WTypes._
  import WFunc._

  def startingEnv = Map(
    // Some pretty primitive stuff
    Symbol("#eval") -> Eval,
    Symbol("#if") -> If,
    Symbol("#vau") -> Vau,
    // Types
    Symbol("#Bool") -> TypeBool,
    Symbol("#Dict") -> TypeDict,
    Symbol("#Num") -> TypeNum,
    Symbol("#Sym") -> TypeSym,
    Symbol("#Type") -> TypeType,
    Symbol("#type-eq") -> TypeEq,
    Symbol("#type-of") -> TypeOf,
    Symbol("#List") -> TypeList,
    // some num stuff
    Symbol("#num-add") -> NumAdd,
    Symbol("#num-div") -> NumDiv,
    Symbol("#num-eq") -> NumEq,
    Symbol("#num-gt") -> NumGreaterThan,
    Symbol("#num-gte") -> NumGreaterThanOrEqual,
    Symbol("#num-lt") -> NumLessThan,
    Symbol("#num-lte") -> NumLessThanOrEqual,
    Symbol("#num-mult") -> NumMult,
    Symbol("#num-neq") -> NumNeq,
    Symbol("#num-sub") -> NumSub,
    Symbol("#num-to-str") -> NumToList,
    // sym stuff
    Symbol("#sym-eq") -> SymEq,
    Symbol("#sym-to-vect") -> SymToVect,
    // vect functions
    Symbol("#list-cons") -> ListCons,
    Symbol("#list-empty") -> ListEmpty,
    Symbol("#list-length") -> ListLength,
    Symbol("#list-make") -> ListMake,
    Symbol("#list-nth") -> ListNth,
    Symbol("#list-reduce") -> ListReduce,
    // Dict functions
    Symbol("#dict-contains") -> DictContains,
    Symbol("#dict-empty") -> Dict(),
    Symbol("#dict-get") -> DictGet,
    Symbol("#dict-insert") -> DictInsert,
    Symbol("#dict-remove") -> DictRemove,
    Symbol("#dict-size") -> DictSize,
    Symbol("#dict-to-list") -> DictToList,
    // boolean
    Symbol("#bool-eq") -> BoolEq,
    Symbol("#bool-false") -> false,
    Symbol("#bool-not") -> BoolNot,
    Symbol("#bool-true") -> true,
    // debug
    Symbol("#error") -> Error,
    Symbol("#trace") -> Trace)
}
