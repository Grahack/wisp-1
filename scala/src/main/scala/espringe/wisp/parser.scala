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
            sys.error("Couldn't FULLY parse. We parsed: " + res + " but didn't get: " + next.source)
        }
        case f: Failure => sys.error("Parser failure: " + f.toString)
        case e: Error => sys.error("Parser error: " + e.toString)
      }
    }

  private def fileParser: Parser[List[W]] = {
    rep(blankLine) ~> repsep(lineParser(0), rep1(blankLine)) ~< rep(blankLine) ~< opt(terminatingBlankLine)
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
  private def terminatingBlankLine = rep(elem(' ') | elem('\t')) ~> opt(comment) ~< opt(eol)

  private def atomParser: Parser[W] =
    ((numParser | charParser | listParser | literalStringParser | literalVectParser | symbolParser | literalDictParser | builtInSymbolParser) ~ opt('.' ~> atomParser) ^^
      (x => if (x._2.isDefined) WList(Stream(x._1, x._2.get)) else x._1))

  private def charParser =
    ('~' ~> acceptIf(!special(_))("expected char, but found: " + _) ^^ (x => new WChar(x)))

  private def listParser =
    ('(' ~> repsep(atomParser, singleSpace) ~< ')' ^^ (x => new WList(x.toStream)))

  private def literalVectParser = {
    ('[' ~> repsep(atomParser, singleSpace) ~< ']' ^^ { x =>
      val mk = BuiltinFunction(BuiltinFunctionNames.ListMake)
      WList(mk #:: (x.toStream: Stream[W]))
    })
  }

  // {key value, key value, key value} 
  private def literalDictParser = ('{' ~> opt(singleSpace) ~> repsep(dictPairParser, ',' ~ singleSpace) ~< opt(singleSpace) <~ '}' ^^
    { x =>
      val dm = BuiltinFunction(BuiltinFunctionNames.DictMake)
      val stream: Stream[W] = x.toStream
      new WList(dm #:: stream)
    })

  private def dictPairParser = atomParser ~< singleSpace ~ atomParser ^^ (x => new WList(Stream(BuiltinFunction(BuiltinFunctionNames.ListMake), x._1, x._2)))

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
    val mk = BuiltinFunction(BuiltinFunctionNames.ListMake)
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

      import BuiltinFunctionNames._

      val ls = LexicalSource("UnknownFile", x.pos.column, x.pos.line)

      x.str match {
        case "true" => Bool(true, ls)
        case "false" => Bool(false, ls)
        case fn =>
          BuiltinFunction(
            fn match {
              case "bool-eq" => BoolEq
              case "bool-not" => BoolNot
              case "dict-contains" => DictContains
              case "dict-get" => DictGet
              case "dict-insert" => DictInsert
              case "dict-make" => DictMake
              case "dict-remove" => DictRemove
              case "dict-size" => DictSize
              case "dict-to-list" => DictToList
              case "error" => BuiltinFunctionNames.Error
              case "eval" => Eval
              case "if" => If
              case "list-cons" => ListCons
              case "list-head" => ListHead
              case "list-empty?" => ListIsEmpty
              case "list-make" => ListMake
              case "list-tail" => ListTail
              case "num-add" => NumAdd
              case "num-div" => NumDiv
              case "num-eq" => NumEq
              case "num-gt" => NumGT
              case "num-gte" => NumGTE
              case "num-lt" => NumLT
              case "num-lte" => NumLTE
              case "num-mult" => NumMult
              case "num-sub" => NumSub
              case "num-to-char-list" => NumToCharList
              case "parse" => Parse
              case "quote" => Quote
              case "read-file" => ReadFile
              case "sym-eq" => SymEq
              case "sym-to-char-list" => SymToCharList
              case "trace" => Trace
              case "type-eq" => TypeEq
              case "type-of" => TypeOf
              case "vau" => Vau
            }, ls)
      }

    }

  private def eol = elem('\n') // probably should support windows-endline, but meh

  private def singleSpace = elem(' ')

  // get around annoying precedent rule of <~
  implicit class UnannoyingParser[T](left: Parser[T]) { def ~<[V](right: => Parser[V]) = left <~ right }

}


