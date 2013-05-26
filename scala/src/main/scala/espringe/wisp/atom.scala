package espringe.wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

sealed trait W {
  def source: SourceInfo
  def deparse: String
  def typeOf: Primitives.Primitive

  def getBoolean: Option[Boolean] = None
  def getChar: Option[Char] = None
  def getDict: Option[Dict] = None
  def getFunctionCall: Option[(W, WList)] = None
  def getLong: Option[Long] = None
  def getSymbol: Option[Symbol] = None
  def getBuiltinFunctionName: Option[BuiltinFunction.Name] = None

  override def hashCode: Int = sys.error(s"Implementation misssing in $this")
  override def equals(o: Any): Boolean = sys.error(s"Implementation misssing in $this")
  override def toString = deparse
}

object Bool {
  def apply(value: Boolean)(implicit source: SourceInfo) = new Bool(value, source)
  def unapply(value: W): Option[Boolean] = value.getBoolean
}

class Bool(val value: Boolean, val source: SourceInfo) extends W {
  override def deparse = if (value) "$true" else "$false"
  override def typeOf = Primitives.TypeBool
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case Bool(b) => value == b
    case b: Boolean => value == b
    case _ => false
  }

  override def getBoolean = Some(value)
}

object WChar {
  def apply(value: Char)(implicit source: SourceInfo) = new WChar(value, source)
  def unapply(value: W): Option[Char] = value.getChar
}
class WChar(val value: Char, val source: SourceInfo) extends W {
  override def deparse = "~" + value
  override def typeOf = Primitives.TypeChar
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case WChar(c) => value == c
    case c: Char => value == c
    case _ => false
  }
  override def getChar = Some(value)
}

object WDict {
  def apply(value: Dict = new HashMap())(implicit source: SourceInfo) = new WDict(value, source)
  def unapply(value: W) = value.getDict
}

class WDict(val value: Dict, val source: SourceInfo) extends W {
  override def deparse =
    "{" + value.flatMap { case (k, v) => Seq(k.deparse, v.deparse) }.mkString(" ") + "}"
  override def typeOf = Primitives.TypeDict
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case WDict(d) => value == d
    case d: Dict => value == d
    case _ => false
  }
  override def getDict = Some(value)
}

object WList {
  def apply(values: Iterable[W])(implicit source: SourceInfo): WList = { // TODO: handle source-info stuff...
    if (values.isEmpty)
      WNil
    else
      new WCons(values.head, WList(values.tail), source)
  }
}

object ~: {
  def unapply(xs: WList): Option[(W, WList)] = xs match {
    case x: WCons => Some((x.head, x.tail))
    case WNil => None
  }
}

sealed trait WList extends Iterable[W] with W {
  def mapW(f: W => W): WList
  def asString: Option[String]
}

object WNil extends WList {
  override def source = new SourceInfo { override def print = "global nil" }

  def unapply(xs: WList): Boolean = xs.isInstanceOf[WNil.type]

  override def deparse = "[]"
  override def typeOf = Primitives.TypeList
  override def hashCode = "WEmpty".hashCode
  override def equals(o: Any) = o.isInstanceOf[WNil.type] || (o match {
    case i: Iterable[_] => i.isEmpty
    case "" => true
    case _ => false
  })
  override def asString = Some("")

  override def iterator = new Iterator[W] {
    override def hasNext = false
    override def next: W = sys.error("Can't call next an iterator of an empty list")
  }
  override def mapW(f: W => W) = WNil
}

object WCons {
  def apply(head: W, rest: => WList)(implicit source: SourceInfo) = new WCons(head, rest, source)
}

class WCons(override val head: W, rest: => WList, val source: SourceInfo) extends WList {
  override def deparse = asLiteralString.getOrElse(asLiteralList)
  override def tail = rest
  override def mapW(f: W => W) = new WCons(f(head), rest.mapW(f), source) // TODO: this source isn't quite right

  private def asLiteralList = {
    val sb = StringBuilder.newBuilder
    sb += '['
    sb ++= head.deparse

    iterate(tail)
    def iterate(at: WList) {
      at match {
        case _: WCons if sb.length > 1000 => // To not spam too much, and/or handle infinite lists
          sb ++= "..."
        case head ~: tail =>
          sb += ' '
          sb ++= head.deparse
          iterate(tail)
        case WNil =>
      }
    }
    sb += ']'
    sb.result
  }

  private def asLiteralString: Option[String] = {
    val sb = StringBuilder.newBuilder
    sb += '"'

    var at: WList = this
    while (!at.isInstanceOf[WNil.type]) {
      at match {
        case WChar(head) ~: tail =>
          if (head == '"' || head == '\\') {
            sb += '\\'
          }
          sb += head
          at = tail
        case _ => return None
      }
    }

    sb += '"'
    Some(sb.result)

  }

  override def asString: Option[String] = {
    val sb = StringBuilder.newBuilder

    var at: WList = this
    while (!at.isInstanceOf[WNil.type]) {
      at match {
        case WChar(head) ~: tail =>
          sb += head
          at = tail
        case _ => return None
      }
    }

    Some(sb.result)
  }

  override def typeOf = Primitives.TypeList
  override def hashCode = head.hashCode ^ tail.hashCode
  override def equals(o: Any) = o match {
    case h ~: t => head == h && tail == t
    case i: Iterable[_] if !i.isEmpty => head == i.head && tail == i.tail
    case str: String => equals(str.toIterable)
    case _ => false
  }

  private def self = this
  override def iterator = new Iterator[W] {
    var at: WList = self
    override def hasNext = at.isInstanceOf[WCons]
    override def next: W = at match {
      case head ~: tail =>
        at = tail
        head
      case _ => sys.error(s"Overrused the iterator, previously was pointed at $at")
    }
  }

}

object Num {
  def apply(value: Long)(implicit source: SourceInfo) = new Num(value, source)
  def unapply(value: W) = value.getLong
}

class Num(val value: Long, val source: SourceInfo) extends W {
  override def deparse = value.toString
  override def typeOf = Primitives.TypeNum
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case Num(n) => value == n
    case i: Int => value == i
    case l: Long => value == l
    case _ => false
  }
  override def getLong = Some(value)
}

object Sym {
  def apply(value: Symbol)(implicit source: SourceInfo) = new Sym(value, source)
  def unapply(value: W): Option[Symbol] = value.getSymbol
}
class Sym(val value: Symbol, val source: SourceInfo) extends W {
  override def deparse = value.name
  override def typeOf = Primitives.TypeSym
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case Sym(s) => value == s
    case s: Symbol => value == s
    case _ => false
  }
  override def getSymbol = Some(value)
}

case class UDF(capEnv: WDict, arg: Sym, env: Sym, capCode: W)(implicit val source: SourceInfo) extends W {
  override def deparse = "$UDF$"
  override def typeOf = Primitives.TypeFunc
  override def hashCode = capEnv.hashCode ^ arg.hashCode ^ env.hashCode ^ capCode.hashCode
  override def equals(o: Any) = o match {
    case UDF(ce, a, e, c) => capEnv == ce && arg == a && env == e && capCode == c
    case _ => false
  }
}

object Primitives extends Enumeration {
  type Primitive = Value
  val TypeApply, TypeBool, TypeChar, TypeSym, TypeNum, TypeDict, TypeBuiltin, TypeFunc, TypeList, TypeType = Value

  def unapply(s: String) = s match {
    case "type-apply" => Some(TypeApply)
    case "type-bool" => Some(TypeBool)
    case "type-char" => Some(TypeChar)
    case "type-sym" => Some(TypeSym)
    case "type-num" => Some(TypeNum)
    case "type-dict" => Some(TypeDict)
    case "type-builtin" => Some(TypeBuiltin)
    case "type-func" => Some(TypeFunc)
    case "type-list" => Some(TypeList)
    case "type-type" => Some(TypeType)
    case _ => None
  }
}

case class WType(value: Primitives.Primitive)(implicit val source: SourceInfo) extends W {
  import Primitives._
  override def deparse = value match {
    case TypeApply => "$type-apply"
    case TypeBool => "$type-bool"
    case TypeChar => "$type-char"
    case TypeSym => "$type-sym"
    case TypeNum => "$type-num"
    case TypeDict => "$type-dict"
    case TypeBuiltin => "$type-builtin"
    case TypeFunc => "$type-func"
    case TypeList => "$type-list"
    case TypeType => "$type-type"
  }
  override def typeOf = Primitives.TypeType
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case WType(v) => value == v
    case p: Primitives.Primitive => value == p
    case _ => false
  }
}

object BuiltinFunction extends Enumeration {
  type Name = Value
  val BoolEq, BoolNot, DictContains, DictGet, DictInsert, DictMake, DictRemove, DictSize, DictToList, Error, Eval, FnCallArgs, FnCallFn, FnCallMake, If, Let, ListCons, ListHead, ListIsEmpty, ListMake, ListTail, NumAdd, NumDiv, NumEq, NumGT, NumGTE, NumLT, NumLTE, NumMult, NumSub, NumToCharList, Parse, Quote, ReadFile, SymEq, SymToCharList, Then, Trace, TypeEq, TypeOf, Vau = Value

  def apply(value: Name)(implicit source: SourceInfo) = new BuiltinFunction(value, source)
  def unapply(value: W) = value.getBuiltinFunctionName

  def unapply(s: String) = s match {
    case "bool-eq" => Some(BoolEq)
    case "bool-not" => Some(BoolNot)
    case "dict-contains" => Some(DictContains)
    case "dict-get" => Some(DictGet)
    case "dict-insert" => Some(DictInsert)
    case "dict-make" => Some(DictMake)
    case "dict-remove" => Some(DictRemove)
    case "dict-size" => Some(DictSize)
    case "dict-to-list" => Some(DictToList)
    case "error" => Some(Error)
    case "eval" => Some(Eval)
    case "fn-call-args" => Some(FnCallArgs)
    case "fn-call-fn" => Some(FnCallFn)
    case "fn-call-make" => Some(FnCallMake)
    case "if" => Some(If)
    case "let" => Some(Let)
    case "list-cons" => Some(ListCons)
    case "list-head" => Some(ListHead)
    case "list-empty?" => Some(ListIsEmpty)
    case "list-make" => Some(ListMake)
    case "list-tail" => Some(ListTail)
    case "num-add" => Some(NumAdd)
    case "num-div" => Some(NumDiv)
    case "num-eq" => Some(NumEq)
    case "num-gt" => Some(NumGT)
    case "num-gte" => Some(NumGTE)
    case "num-lt" => Some(NumLT)
    case "num-lte" => Some(NumLTE)
    case "num-mult" => Some(NumMult)
    case "num-sub" => Some(NumSub)
    case "num-to-char-list" => Some(NumToCharList)
    case "parse" => Some(Parse)
    case "quote" => Some(Quote)
    case "read-file" => Some(ReadFile)
    case "sym-eq" => Some(SymEq)
    case "sym-to-char-list" => Some(SymToCharList)
    case "then" => Some(Then)
    case "trace" => Some(Trace)
    case "type-eq" => Some(TypeEq)
    case "type-of" => Some(TypeOf)
    case "vau" => Some(Vau)
    case _ => None
  }
}

class BuiltinFunction(val value: BuiltinFunction.Name, val source: SourceInfo) extends W {
  import BuiltinFunction._

  override def deparse = value match {
    case BoolEq => "$bool-eq"
    case BoolNot => "$bool-not"
    case DictContains => "$dict-contains"
    case DictGet => "$dict-get"
    case DictMake => "$dict-make"
    case DictInsert => "$dict-insert"
    case DictRemove => "$dict-remove"
    case DictSize => "$dict-size"
    case DictToList => "$dict-list"
    case Error => "$error"
    case Eval => "$eval"
    case FnCallArgs => "$fn-call-args"
    case FnCallFn => "$fn-call-fn"
    case FnCallMake => "$fn-call-make"
    case If => "$if"
    case Let => "$let"
    case ListCons => "$list-cons"
    case ListHead => "$list-head"
    case ListIsEmpty => "$list-empty?"
    case ListMake => "$list-make"
    case ListTail => "$list-tail"
    case NumAdd => "$num-add"
    case NumDiv => "$num-div"
    case NumEq => "$num-eq"
    case NumGT => "$num-gt"
    case NumGTE => "$num-gte"
    case NumLT => "$num-lt"
    case NumLTE => "$num-lte"
    case NumMult => "$num-mult"
    case NumSub => "$num-sub"
    case NumToCharList => "$num-to-char-list"
    case Parse => "$parse"
    case Quote => "$quote"
    case ReadFile => "$read-file"
    case SymEq => "$sym-eq"
    case SymToCharList => "$sym-eq"
    case Trace => "$trace"
    case TypeEq => "$type-eq"
    case TypeOf => "$type-of"
    case Vau => "$vau"

  }
  override def typeOf = Primitives.TypeBuiltin

  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case bf: BuiltinFunction => value == bf.value
    case n: BuiltinFunction.Name => value == n
    case _ => false
  }

  override def getBuiltinFunctionName = Some(value)
}

object FnCall {
  def apply(func: W, args: WList)(implicit source: SourceInfo) = new FnCall(func, args, source)
  def unapply(value: W) = value.getFunctionCall
}

class FnCall(val func: W, val args: WList, val source: SourceInfo) extends W {
  override def typeOf = Primitives.TypeApply
  override def deparse = hasOneArg.map(arg => func.deparse + "." + arg.deparse)
    .getOrElse("(" + func.deparse + args.map(" " + _.deparse).mkString + ")")

  override def hashCode = "WApply".hashCode ^ func.hashCode ^ args.hashCode
  override def equals(o: Any) = o match {
    case FnCall(f, a) => func == f && args == a
    case (f, a) => func == f && args == a
    case _ => false
  }

  private def hasOneArg: Option[W] = (func, args) match {
    case (f: FnCall, _) if f.hasOneArg.isDefined => None
    case (_, single ~: WNil()) => Some(single)
    case _ => None
  }

  override def getFunctionCall = Some((func, args))
}

// This is a state machine, that switches from unevaled to eval and forwards
// all requests
class Lazy(var what: W)(implicit val source: SourceInfo) extends W {

  private var evaler: W => W = null
  private var state: Char = 0

  def setEvaler(fn: W => W) {
    synchronized {
      assert(state == 0 && evaler == null)
      evaler = fn
      state = 1
    }
  }

  override def deparse = get().deparse
  override def typeOf = get().typeOf

  override def hashCode: Int = get().hashCode
  override def equals(o: Any): Boolean = get() == o

  def get() = synchronized {
    state match {
      case 0 => sys.error("Lazy object has not had evaler set yet")
      case 1 => // unevalued
        assert(evaler != null)
        state = 2 // in evaluation
        what = evaler(what)
        evaler = null // this is no longer needed, free the reference for gc
        state = 3 // finished evaluation
        what
      case 2 => // in evaluation
        sys.error(s"Trying to evaluate object $what while its been evaluated")
      case 3 => // finished evaluation
        what
    }
  }

  override def toString = state match {
    case 0 => s"%L0[$what]"
    case 1 => s"%L1[$what]"
    case 2 => s"%L2[$what]"
    case 3 => what.toString
  }

  override def getBoolean = get().getBoolean
  override def getChar = get().getChar
  override def getDict = get().getDict
  override def getLong = get().getLong
  override def getSymbol = get().getSymbol
  override def getBuiltinFunctionName = get().getBuiltinFunctionName
  override def getFunctionCall = get().getFunctionCall
}


