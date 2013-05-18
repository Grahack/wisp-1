package espringe.wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

sealed trait SourceInfo {
  def print: String
}
object UnknownSource extends SourceInfo {
  def print = "Source: Unknown"
}

case class LexicalSource(file: String, column: Long, line: Long) extends SourceInfo {
  def print = s"Source: $file column $column line $line"
}
class ComputedSource(from: W) extends SourceInfo {
  def print = "Computed from: \n\t" + from.toString.replaceAll("\n", "\n\t")
}

sealed abstract class W(source: SourceInfo) {
  def deparse: String
  def typeOf: Primitives.Primitive

  def asBuiltin: Option[BuiltinFunctionNames.Name] = None
  def asBool: Option[Boolean] = None
  def asChar: Option[Char] = None
  def asDict: Option[Dict] = None
  def asList: Option[Iterable[W]] = None
  def asNum: Option[Long] = None
  def asSym: Option[Symbol] = None
  def asType: Option[Primitives.Primitive] = None

  def asString: Option[String] = None

  override def hashCode: Int = sys.error(s"Implementation misssing in $this")
  override def equals(o: Any): Boolean = sys.error(s"Implementation misssing in $this")
  override def toString = deparse
}

case class Bool(value: Boolean, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = if (value) "#true" else "#false"
  override def typeOf = Primitives.TypeBool
  override def asBool = Some(value)
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case Bool(b, _) => value == b
    case b: Boolean => value == b
    case _ => false
  }
}

case class WChar(value: Char, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "~" + value
  override def typeOf = Primitives.TypeChar
  override def asChar = Some(value)
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case WChar(c, _) => value == c
    case c: Char => value == c
    case _ => false
  }
}

case class WDict(value: Dict, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse =
    "{" + value.toList.map(x => (x._1.toString + " " + x._2.toString)).mkString(", ") + "}"
  override def typeOf = Primitives.TypeDict
  override def asDict = Some(value)
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case WDict(d, _) => value == d
    case d: Dict => value == d
    case _ => false
  }
}

object WList {
  def apply(values: Iterable[W]): WList = { // TODO: handle source-info stuff...
    if (values.isEmpty)
      WEmpty()
    else
      new WCons(values.head, WList(values.tail))
  }
}
object WNil {
  def unapply(xs: WList): Boolean = xs.isInstanceOf[WEmpty]
}
object ~: {
  def unapply(xs: WList): Option[(W, WList)] = xs match {
    case x: WCons => Some((x.head, x.tail))
    case WEmpty(_) => None
  }
}

sealed trait WList extends W with Iterable[W]

case class WEmpty(source: SourceInfo = UnknownSource) extends W(source) with WList {
  override def deparse = "[]"
  override def typeOf = Primitives.TypeList
  override def hashCode = "WEmpty".hashCode
  override def equals(o: Any) = o match {
    case WEmpty(_) => true
    case i: Iterable[_] => i.isEmpty
    case "" => true
    case _ => false
  }
  override def asString = Some("")

  override def iterator = new Iterator[W] {
    override def hasNext = false
    override def next: W = sys.error("Can't call next an iterator of an empty list")
  }
  override def asList = Some(Iterable())
}

class WCons(override val head: W, rest: => WList, source: SourceInfo = UnknownSource) extends W(source) with WList {
  override def deparse = {
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
        case WEmpty(_) =>
      }
    }
    sb += ']'
    sb.result
  }
  override def tail = rest

  override def asString: Option[String] = {
    val sb = StringBuilder.newBuilder

    var at: WList = this
    while (true) {
      at match {
        case WChar(head, _) ~: tail =>
          sb += head
          at = tail
        case WEmpty(_) =>
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

  override def asList = Some(iterator.toIterable)
}

case class Num(value: Long, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = value.toString
  override def typeOf = Primitives.TypeNum
  override def asNum = Some(value)
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case Num(n, _) => value == n
    case i: Int => value == i
    case l: Long => value == l
    case _ => false
  }
}

case class Sym(value: Symbol, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = value.name
  override def typeOf = Primitives.TypeSym
  override def asSym = Some(value)
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case Sym(s, _) => value == s
    case s: Symbol => value == s
    case _ => false
  }
}

case class UDF(capEnv: Dict, arg: Symbol, env: Symbol, capCode: W, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#???UDF???"
  override def typeOf = Primitives.TypeFunc
  override def hashCode = capEnv.hashCode ^ arg.hashCode ^ env.hashCode ^ capCode.hashCode
  override def equals(o: Any) = o match {
    case UDF(ce, a, e, c, _) => capEnv == ce && arg == a && env == e && capCode == c
    case _ => false
  }
}

object Primitives extends Enumeration {
  type Primitive = Value
  val TypeApply, TypeBool, TypeChar, TypeSym, TypeNum, TypeDict, TypeBuiltIn, TypeFunc, TypeList, TypeType = Value
}

case class WType(value: Primitives.Primitive, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "{Type: " + value.toString + "}"
  override def typeOf = Primitives.TypeType
  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case WType(v, _) => value == v
    case p: Primitives.Primitive => value == p
    case _ => false
  }
}

object BuiltinFunctionNames extends Enumeration {
  type Name = Value
  val BoolEq, BoolNot, DictContains, DictGet, DictInsert, DictMake, DictRemove, DictSize, DictToList, Error, Eval, If, ListCons, ListHead, ListIsEmpty, ListMake, ListTail, NumAdd, NumDiv, NumEq, NumGT, NumGTE, NumLT, NumLTE, NumMult, NumSub, NumToCharList, Parse, Quote, ReadFile, SymEq, SymToCharList, Trace, TypeEq, TypeOf, Vau = Value
}


case class FuncCall(func: W, args: WList, source: SourceInfo = UnknownSource) extends W(source)  {
  override def typeOf = Primitives.TypeApply
  override def deparse = "(" + func.deparse + args.map(" " + _.deparse).mkString  + ")"
  override def hashCode = "WApply".hashCode ^ func.hashCode ^ args.hashCode
  override def equals(o: Any) = o match {
    case FuncCall(f, a, _) => func == f && args == a
    case (f, a) => func == f && args == a 
    case _ => false
  }
}

case class BuiltinFunction(value: BuiltinFunctionNames.Name, source: SourceInfo = UnknownSource) extends W(source) {
  import BuiltinFunctionNames._

  override def deparse = value match {
    case BoolEq => "#bool-eq"
    case BoolNot => "#bool-not"
    case DictContains => "#dict-contains"
    case DictGet => "#dict-get"
    case DictMake => "#dict-make"
    case DictInsert => "#dict-insert"
    case DictRemove => "#dict-remove"
    case DictSize => "#dict-size"
    case DictToList => "#dict-list"
    case Error => "#error"
    case Eval => "#eval"
    case If => "#if"
    case ListCons => "#list-cons"
    case ListHead => "#list-head"
    case ListIsEmpty => "#list-empty?"
    case ListMake => "#list-make"
    case ListTail => "#list-tail"
    case NumAdd => "#num-add"
    case NumDiv => "#num-div"
    case NumEq => "#num-eq"
    case NumGT => "#num-gt"
    case NumGTE => "#num-gte"
    case NumLT => "#num-lt"
    case NumLTE => "#num-lte"
    case NumMult => "#num-mult"
    case NumSub => "#num-sub"
    case NumToCharList => "#num-to-char-list"
    case Parse => "#parse"
    case Quote => "#quote"
    case ReadFile => "#read-file"
    case SymEq => "#sym-eq"
    case SymToCharList => "#sym-eq"
    case Trace => "#trace"
    case TypeEq => "#type-eq"
    case TypeOf => "#type-of"
    case Vau => "#vau"

  }
  override def typeOf = Primitives.TypeBuiltIn
  override def asBuiltin = Some(value)

  override def hashCode = value.hashCode
  override def equals(o: Any) = o match {
    case BuiltinFunction(bf, _) => value == bf
    case n: BuiltinFunctionNames.Name => value == n
    case _ => false
  }
}
