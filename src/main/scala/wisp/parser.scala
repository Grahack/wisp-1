package wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import scala.collection.immutable.HashMap

import java.nio.file._

object Parser extends Parsers {

  import scala.util.parsing.input.Positional

  type Elem = Char

  def apply(path: scala.io.Source): WList with Positional =
    run(new scala.util.parsing.input.PagedSeqReader(
      scala.collection.immutable.PagedSeq.fromSource(path)))

  def apply(contents: CharSequence): WList with Positional = run(new scala.util.parsing.input.CharSequenceReader(contents))

  def run(rdr: scala.util.parsing.input.Reader[Char]) = {
    fileParser(rdr) match {
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

  private def fileParser: Parser[WList with Positional] = {
    positioned(rep(eol) ~> repsep(lineParser(0), rep1(eol)) ~< rep(eol) ^^ (x => new WList(x.toStream) with Positional))
  }

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
    positioned((numParser | charParser | listParser | literalStringParser | literalVectParser | symbolParser | literalDictParser | builtInSymbolParser) ~ opt('.' ~> atomParser) ^^
      (x => if (x._2.isDefined) new WList(Stream(x._1, x._2.get)) with Positional else x._1))

  private def charParser =
    positioned('~' ~> acceptIf(!special(_))("expected char, but found: " + _) ^^ (x => new WChar(x) with Positional))

  private def listParser =
    positioned('(' ~> repsep(atomParser, singleSpace) ~< ')' ^^ (x => new WList(x.toStream) with Positional))

  private def literalVectParser = {
    positioned('[' ~> repsep(atomParser, singleSpace) ~< ']' ^^ { x =>
      val mk = new ListMake {}
      new WList(mk #:: (x.toStream: Stream[W])) with Positional
    })
  }

  // {key value, key value, key value} 
  private def literalDictParser = positioned('{' ~> opt(singleSpace) ~> repsep(dictPairParser, ',' ~ singleSpace) ~< opt(singleSpace) <~ '}' ^^
    (x => new Dict(listToHashMap(x.map(y => y._1 -> y._2))) with Positional))

  private def dictPairParser = atomParser ~< singleSpace ~ atomParser

  private def listToHashMap[A, B](values: List[(A, B)]): HashMap[A, B] = values.foldLeft(HashMap[A, B]()) {
    (state, next) =>
      require(!state.contains(next._1), next + " already exists in literal dict")
      state + next
  }

  // TODO: allow arbitrary base
  private def numParser = positioned(rep1(digitParser) ^^ (x => new Num(numberListToNumber(x, base = 10)) with Positional))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (_.asDigit)

  //  private def builtInParser = WBuiltIn.all.map {
  //    x => accept( x.name.name.toList) ^^ (_ => x)
  //  }.reduce((a,b) => a|b)
  //

  private def symbolParser = positioned(rep1(
    acceptIf(!special(_))(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ (x => new Sym(charListToSymbol(x)) with Positional))

  private def special(c: Char) =
    c.isWhitespace || c.isControl ||
      c == '(' || c == ')' ||
      c == '[' || c == ']' ||
      c == '~' || c == '"' ||
      c == ';' || c == '.' ||
      c == ',' || c == '#' ||
      c == '{' || c == '}'

  private def literalStringParser = '"' ~> rep(positioned(insideLiteralParser ^^ (new WChar(_) with Positional))) ~< '"' ^^ { x =>
    val mk = new ListMake {}
    new WList(mk #:: (x.toStream: Stream[W])) with Positional
   }

  // TODO: allow string escaping
  private def insideLiteralParser = acceptIf(x => x != '"' && x != '\n')("Unexpected '" + _ + "' when parsing inside a literal string")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0) { (acc: Int, value: Int) => acc * base + value }

  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private def eol = elem('\n') // warn if retarded-end line found?

  private def singleSpace = elem(' ')

  // get around annoying precedent rule of <~
  import language.implicitConversions
  implicit private def toUnannoying[T](p: Parser[T]): UnannoyingParser[T] = new UnannoyingParser(p)
  private class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

  private def bm(s: String, exp: => W with Positional): Parser[W with Positional] =
    acceptSeq(s) ^^ (_ => exp)

  private def builtInSymbolParser = '#' ~> (
    bm("True", new Bool(true) with Positional) |
    bm("False", new Bool(false) with Positional) |
    bm("do", new Do with Positional) |
    bm("eval", new Eval with Positional) |
    bm("if", new If with Positional) |
    bm("quote", new Quote with Positional) |
    bm("parse", new Parse with Positional) |
    bm("read-file", new ReadFile with Positional) |
    bm("vau", new Vau with Positional) |
    bm("weave", new Weave with Positional) |
    bm("type-eq", new TypeEq with Positional) |
    bm("type-of", new TypeOf with Positional) |
    bm("bool-not", new BoolNot with Positional) |
    bm("bool-eq", new BoolEq with Positional) |
    bm("num-add", new NumAdd with Positional) |
    bm("num-div", new NumDiv with Positional) |
    bm("num-gt", new NumGT with Positional) |
    bm("num-gte", new NumGTE with Positional) |
    bm("num-eq", new NumEq with Positional) |
    bm("num-lt", new NumLT with Positional) |
    bm("num-lte", new NumLTE with Positional) |
    bm("num-mult", new NumMult with Positional) |
    bm("num-sub", new NumSub with Positional) |
    bm("num-to-char-list", new NumToCharList with Positional) |
    bm("sym-eq", new SymEq with Positional) |
    bm("sym-to-char-list", new SymToCharList with Positional) |
    bm("list-cons", new ListCons with Positional) |
    bm("list-head", new ListHead with Positional) |
    bm("list-empty?", new ListIsEmpty with Positional) |
    bm("list-make", new ListMake with Positional) |
    bm("list-tail", new ListTail with Positional) |
    bm("dict-contains", new DictContains with Positional) |
    bm("dict-get", new DictGet with Positional) |
    bm("dict-insert", new DictInsert with Positional) |
    bm("dict-remove", new DictRemove with Positional) |
    bm("dict-size", new DictSize with Positional) |
    bm("dict-to-list", new DictToList with Positional) |
    bm("trace", new Trace with Positional) |
    bm("error", new WError with Positional))
}
