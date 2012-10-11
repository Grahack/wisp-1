package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import java.nio.file._

object Reader extends Parsers {

  import scala.util.parsing.input.Positional

  type Elem = Char

  def apply(path: Path): Seq[W] = apply(new String(Files.readAllBytes(path)))

  def apply(contents: String): Seq[W with Positional] = {

    val csr = new scala.util.parsing.input.CharArrayReader(contents.toCharArray())

    fileParser(csr) match {
      case Success(res, next) => {
        if (next.atEnd)
          res
        else
          sys.error("Couldn't FULLY parse. We parsed: " + res + " but didn't get: " + next)
      }
      case f: Failure => sys.error("Parser failure: " + f.toString)
      case e: Error => sys.error("Parser error: " + e.toString)
    }
  }

  private def fileParser: Parser[List[W with Positional]] =
    rep(lineParser(0)) ~< rep(eol)

  private def lineParser(depth: Int): Parser[W with Positional] =
    rep(blankLine) ~>
      repN(depth, '\t') ~>
      positioned(
        rep1sep(atomParser, singleSpace) ~
          rep(lineParser(depth + 1)) ^^ (x => stitch(x._1, x._2)))

  private def stitch(a: Seq[W with Positional], b: Seq[W with Positional]) = {
    assert(a.length >= 1)
    if (a.length == 1 && b.length == 0)
      a.head
    else
      new WList(a.toStream ++ b.toStream) with Positional
  }

  private def comment = ';' ~> rep(acceptIf(_ != '\n')("Didn't expect: " + _ + " in comment"))
  private def blankLine = rep(elem(' ') | elem('\t')) ~> opt(comment) ~< eol

  private def atomParser: Parser[W with Positional] =
    positioned((numParser | charParser | listParser | literalStringParser | literalListParser | builtInSymbolParser | symbolParser) ~ opt('.' ~> atomParser) ^^
      (x => if (x._2.isDefined) new WList(Stream(x._1, x._2.get)) with Positional else x._1))

  private def charParser =
    positioned('~' ~> acceptIf(!special(_))("expected char, but found: " + _) ^^ (x => new WChar(x) with Positional))

  private def listParser =
    positioned('(' ~> repsep(atomParser, singleSpace) ~< ')' ^^ (x => new WList(x.toStream) with Positional))

  private def literalListParser = {
    val tmp = new ListMake {} // compiler bug work-around
    positioned('[' ~> repsep(atomParser, singleSpace) ~< ']' ^^ (x => new WList(tmp #:: (x.toStream: Stream[W])) with Positional))
  }

  // TODO: allow arbitrary base
  private def numParser = positioned(rep1(digitParser) ^^ (x => new WNum(numberListToNumber(x, base = 10)) with Positional))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (_.asDigit)

  //  private def builtInParser = WBuiltIn.all.map {
  //    x => accept( x.name.name.toList) ^^ (_ => x)
  //  }.reduce((a,b) => a|b)
  //

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

  private def charListToStream(letters: List[Char]) = new ListMake {} #:: (letters.map(new WChar(_)).toStream: Stream[W])
  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private def eol = elem('\n') // warn if retarded-end line found?

  private def singleSpace = elem(' ')

  // get around annoying precedent rule of <~
  import scala.language.implicitConversions
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

  private def bm(s: String, exp: => W with Positional): Parser[W with Positional] =
    acceptSeq(s) ^^ (_ => exp)

  private def builtInSymbolParser =
    bm("#True", new WBool(true) with Positional) |
      bm("#False", new WBool(false) with Positional) | 
      bm("#eval", new WEval with Positional) | 
      bm("#if", new WIf with Positional) | 
      bm("#ast", new AstOf with Positional) | 
      bm("#type-eq", new TypeEq with Positional) | 
      bm("#type-of", new TypeOf with Positional) | 
      bm("#bool-not", new BoolNot with Positional) | 
      bm("#bool-eq", new BoolEq with Positional) | 
      bm("#num-add", new NumAdd with Positional) | 
      bm("#num-div", new NumDiv with Positional) | 
      bm("#num-gt", new NumGT with Positional) | 
      bm("#num-gte", new NumGTE with Positional) | 
      bm("#num-eq", new NumEq with Positional) | 
      bm("#num-lt", new NumLT with Positional) | 
      bm("#num-lte", new NumLTE with Positional) | 
      bm("#num-mult", new NumMult with Positional) | 
      bm("#num-sub", new NumSub with Positional) | 
      bm("#num-to-char-list", new NumToCharList with Positional) | 
      bm("#sym-eq", new SymEq with Positional) | 
      bm("#sym-to-char-list", new SymToCharList with Positional) | 
      bm("#list-cons", new ListCons with Positional) | 
      bm("#list-head", new ListHead with Positional) | 
      bm("#list-empty?", new ListIsEmpty with Positional) | 
      bm("#list-length", new ListLength with Positional) | 
      bm("#list-make", new ListMake with Positional) | 
      bm("#list-nth", new ListNth with Positional) | 
      bm("#list-tail", new ListTail with Positional) | 
      bm("#dict-contains", new DictContains with Positional) | 
      bm("#dict-get", new DictGet with Positional) | 
      bm("#dict-insert", new DictInsert with Positional) | 
      bm("#dict-remove", new DictRemove with Positional) | 
      bm("#dict-size", new DictSize with Positional) | 
      bm("#dict-to-list", new DictToList with Positional) | 
      bm("#trace", new DictContains with Positional) | 
      bm("#error", new DictContains with Positional)
}
