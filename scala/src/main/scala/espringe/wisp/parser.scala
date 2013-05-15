package espringe.wisp

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

import java.io._

object Parser extends Parsers {

  type Elem = Char

  def apply(path: io.Source): List[W] =
    run(new util.parsing.input.PagedSeqReader(
      collection.immutable.PagedSeq.fromSource(path)))

  def apply(contents: CharSequence): List[W] = run(new util.parsing.input.CharSequenceReader(contents))

  def run(rdr: scala.util.parsing.input.Reader[Char]) =
    this.synchronized { // Parser combinators are unfortunately thread-unsafe?! (SI-4929)
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

  private def fileParser: Parser[List[W]] = {
    rep(blankLine) ~> repsep(lineParser(0), rep1(blankLine)) ~< rep(blankLine)
  }

  private def lineParser(depth: Int): Parser[W] =
    repN(depth, '\t') ~>
      (
        rep1sep(atomParser, singleSpace) ~
        rep(rep1(blankLine) ~> lineParser(depth + 1)) ^^ (x => stitch(x._1, x._2)))

  private def stitch(a: Seq[W], b: Seq[W]) = {
    assert(a.length >= 1)
    if (a.length == 1 && b.length == 0)
      a.head
    else
      WList(a.toStream ++ b.toStream)
  }

  private def comment = ';' ~> rep(acceptIf(_ != '\n')("Didn't expect: " + _ + " in comment"))
  private def blankLine = rep(elem(' ') | elem('\t')) ~> opt(comment) ~< eol

  private def atomParser: Parser[W] =
    ((numParser | charParser | listParser | literalStringParser | literalVectParser | symbolParser | literalDictParser | builtInSymbolParser) ~ opt('.' ~> atomParser) ^^
      (x => if (x._2.isDefined) WList(Stream(x._1, x._2.get)) else x._1))

  private def charParser =
    ('~' ~> acceptIf(!special(_))("expected char, but found: " + _) ^^ (x => new WChar(x)))

  private def listParser =
    ('(' ~> repsep(atomParser, singleSpace) ~< ')' ^^ (x => new WList(x.toStream)))

  private def literalVectParser = {
    ('[' ~> repsep(atomParser, singleSpace) ~< ']' ^^ { x =>
      val mk = new ListMake
      WList(mk #:: (x.toStream: Stream[W]))
    })
  }

  // {key value, key value, key value} 
  private def literalDictParser = ('{' ~> opt(singleSpace) ~> repsep(dictPairParser, ',' ~ singleSpace) ~< opt(singleSpace) <~ '}' ^^
    { x =>
      val dm = new DictMake()
      val stream: Stream[W] = x.toStream
      new WList(dm #:: stream)
    })

  private def dictPairParser = atomParser ~< singleSpace ~ atomParser ^^ (x => new WList(Stream(new ListMake {}, x._1, x._2)))

  private def listToHashMap[A, B](values: List[(A, B)]): HashMap[A, B] = values.foldLeft(HashMap[A, B]()) {
    (state, next) =>
      require(!state.contains(next._1), next + " already exists in literal dict")
      state + next
  }

  // TODO: allow arbitrary base
  private def numParser = (rep1(digitParser) ^^ (x => new Num(numberListToNumber(x, base = 10))))

  private def digitParser: Parser[Int] =
    acceptIf(c => c.isDigit)(c => "Unexpected '" + c + "' when looking for a digit") ^^ (_.asDigit)

  private def symbolParser = (rep1(
    acceptIf(!special(_))(c => "Unexpected '" + c + "' when looking for symbol char")) ^^ (x => new Sym(charListToSymbol(x))))

  private def special(c: Char) =
    c.isWhitespace || c.isControl ||
      c == '(' || c == ')' ||
      c == '[' || c == ']' ||
      c == '~' || c == '"' ||
      c == ';' || c == '.' ||
      c == ',' || c == '#' ||
      c == '{' || c == '}'

  private def literalStringParser = '"' ~> rep((insideLiteralParser ^^ (new WChar(_)))) ~< '"' ^^ { x =>
    val mk = new ListMake {}
    new WList(mk #:: (x.toStream: Stream[W]))
  }

  // TODO: allow string escaping
  private def insideLiteralParser = acceptIf(x => x != '"' && x != '\n')("Unexpected '" + _ + "' when parsing inside a literal string")

  private def numberListToNumber(nums: List[Int], base: Int) =
    nums.foldLeft(0l) { (acc: Long, value: Int) => acc * base + value }

  private def charListToSymbol(letters: List[Char]) = Symbol(new String(letters.toArray))

  private case class PositionalString(str: String) extends Positional

  private def nonSpecialChar = acceptIf(!special(_))(c => "Unexpected '" + c + "' when looking for builtin symbol")

  private def builtInSymbolParser: Parser[W] = positioned('#' ~> rep1(nonSpecialChar) ^^ (x => PositionalString(x.mkString))) ^^
    { x =>

      val p = LexicalSource("UnknownFile", x.pos.column, x.pos.line)

      x.str match {
        case "true" => new Bool(true, p)
        case "false" => new Bool(false, p)
        case "if" => new If(p)
        case "eval" => new Eval(p)
        case "read" => new ReadFile(p)
        case "parse" => new Parse(p)
        case "vau" => new Vau(p)
        case "deref" => new Deref(p)
        case "type-eq" => new TypeEq(p)
        case "type-of" => new TypeOf(p)
        case "bool-not" => new BoolNot(p)
        case "bool-eq" => new BoolEq(p)
        case "list-cons" => new ListCons(p)
        case "list-head" => new ListHead(p)
        case "list-empty?" => new ListIsEmpty(p)
        case "list-make" => new ListMake(p)
        case "list-tail" => new ListTail(p)
        case "num-add" => new NumAdd(p)
        case "num-div" => new NumDiv(p)
        case "num-gt" => new NumGT(p)
        case "num-gte" => new NumGTE(p)
        case "num-eq" => new NumEq(p)
        case "num-lt" => new NumLT(p)
        case "num-lte" => new NumLTE(p)
        case "num-mult" => new NumMult(p)
        case "num-sub" => new NumSub(p)
        case "num-to-char-list" => new NumToCharList(p)
        case "sym-eq" => new SymEq(p)
        case "sym-to-char-list" => new SymToCharList(p)
        case "dict-contains" => new DictContains(p)
        case "dict-get" => new DictGet(p)
        case "dict-insert" => new DictInsert(p)
        case "dict-size" => new DictSize(p)
        case "dict-to-list" => new DictToList(p)
        case "dict-make" => new DictMake(p)
        case "trace" => new Trace(p)
        case "error" => new WError(p)
      }

    }

  private def eol = elem('\n') // probably should support windows-endline, but meh

  private def singleSpace = elem(' ')

  // get around annoying precedent rule of <~
  implicit class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

}


