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

  def asBool: Option[Boolean] = None
  def asChar: Option[Char] = None
  def asDict: Option[Dict] = None
  def asList: Option[Stream[W]] = None
  def asNum: Option[Num] = None
  def asSym: Option[Sym] = None
  def asType: Option[Primitives.Primitive] = None
  def asStream: Option[Stream[W]] = None

  override def hashCode: Int = ???
  override def toString = deparse
}

case class Bool(value: Boolean, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = if (value) "#true" else "#false"
  override def typeOf = Primitives.TypeBool
}

case class WChar(value: Char, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "~" + value
  override def typeOf = Primitives.TypeChar
}

case class WDict(value: Dict, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse =
    "{" + value.toList.map(x => (x._1.toString + " " + x._2.toString)).mkString(", ") + "}"
  override def typeOf = Primitives.TypeDict
}

case class WList(value: Stream[W], source: SourceInfo = UnknownSource) extends W(source) {

  override def equals(o: Any) = o.isInstanceOf[WList] && value == o.asInstanceOf[WList].value

  override def deparse = asString.map(x => s"'$x'")
    .getOrElse("(" + value.map(_.toString).mkString(" ") + ")")
  override def typeOf = Primitives.TypeList

  override def hashCode = 0

  private def asString: Option[String] = {
    val sb = StringBuilder.newBuilder

    value.foreach { c =>
      if (c.isInstanceOf[WChar])
        sb += c.asInstanceOf[WChar].value
      else
        return None
    }

    Some(sb.result)
  }
}

case class Num(value: Long, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = value.toString
  override def typeOf = Primitives.TypeNum
}

case class Sym(value: Symbol, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = value.name
  override def typeOf = Primitives.TypeSym
}

case class UDF(capEnv: Dict, arg: Sym, env: Sym, capCode: W, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#???UDF???"
  override def typeOf = Primitives.TypeFunc
}

object Primitives extends Enumeration {
  type Primitive = Value
  val TypeBool, TypeChar, TypeSym, TypeNum, TypeDict, TypeBuiltIn, TypeFunc, TypeList, TypeType = Value
}

case class WType(value: Primitives.Primitive, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "{Type: " + value.toString + "}"
  override def typeOf = Primitives.TypeType
}

object BuiltinFunctionNames extends Enumeration {
  type Name = Value
  val BoolNot, BoolEq, Deref,
  DictContains, DictGet, DictInsert, DictRemove, DictSize, DictToList, DictMake,
  Error, Eval, If, ListCons,
  ListHead, ListIsEmpty, ListMake, ListTail,
  NumAdd, NumDiv, NumGT, NumGTE, NumEq, NumLT, NumLTE, NumMult, NumSub, NumToCharList,
  SymEq, SymToCharList, Trace, TypeEq, TypeOf, Vau, ReadFile, Parse = Value
}

case class BuiltinFunction(which: BuiltinFunctionNames.Name, source: SourceInfo = UnknownSource) extends W(source) {
  import BuiltinFunctionNames._
 
  override def deparse = which match {
    case If => "#if"
    case Eval => "#eval"
    case ReadFile => "#read"
    case Parse => "#parse"
    case Vau => "#vau"
    case Deref => "#deref"
    case TypeEq => "#type-eq"
    case TypeOf => "#type-of"
    case BoolNot => "#bool-not"
    case BoolEq => "#bool-eq"
    case ListCons => "#list-cons"
    case ListHead => "#list-head"
    case ListIsEmpty => "#list-empty?"
    case ListMake => "#list-make"
    case ListTail => "#list-tail"
    case NumAdd => "#num-add"
    case NumDiv => "#num-div"
    case NumGT => "#num-gt"
    case NumGTE => "#num-gte"
    case NumEq => "#num-eq"
    case NumLT => "#num-lt"
    case NumLTE => "#num-lte"
    case NumMult => "#num-mult"
    case NumSub => "#num-sub"
    case NumToCharList => "#num-to-char-list"
    case SymEq => "#sym-eq"
    case SymToCharList => "#sym-eq"
    case DictContains => "#dict-contains"
    case DictGet => "#dict-get"
    case DictInsert => "#dict-insert"
    case DictSize => "#dict-size"
    case DictToList => "#dict-list"
    case DictMake => "#dict-make"
    case Trace => "#trace"
    case Error => "#error"
  }
  override def typeOf = Primitives.TypeBuiltIn
}
