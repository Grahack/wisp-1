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

  def value: Any
  def deparse: String
  def typeOf: Primitives.Primitive

  def asBuiltin: Option[BuiltinFunctionNames.Name] = None
  def asBool: Option[Boolean] = None
  def asChar: Option[Char] = None
  def asDict: Option[Dict] = None
  def asList: Option[Stream[W]] = None
  def asNum: Option[Long] = None
  def asSym: Option[Symbol] = None
  def asType: Option[Primitives.Primitive] = None

  override def hashCode: Int = value.hashCode
  override def equals(o: Any) = (o.isInstanceOf[W] && value == o.asInstanceOf[W].value) || value == o
  override def toString = deparse
}

case class Bool(value: Boolean, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = if (value) "#true" else "#false"
  override def typeOf = Primitives.TypeBool
  override def asBool = Some(value)
}

case class WChar(value: Char, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "~" + value
  override def typeOf = Primitives.TypeChar
  override def asChar = Some(value)
}

case class WDict(value: Dict, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse =
    "{" + value.toList.map(x => (x._1.toString + " " + x._2.toString)).mkString(", ") + "}"
  override def typeOf = Primitives.TypeDict
  override def asDict = Some(value)
}

case class WList(value: Stream[W], source: SourceInfo = UnknownSource) extends W(source) {

  override def deparse = asString.map(x => '"' + x + '"')
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

  override def asList = Some(value)
  
  override def equals(o: Any) = o match {
    case w: W => value == w.value
    case s: Seq[_] => value == s
    case str: String => asString.map(_ == str).getOrElse(false)
    case _ => false
  }
  
}

case class Num(value: Long, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = value.toString
  override def typeOf = Primitives.TypeNum
  override def asNum = Some(value)
}

case class Sym(value: Symbol, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = value.name
  override def typeOf = Primitives.TypeSym
  override def asSym = Some(value)
}

case class UDF(capEnv: Dict, arg: Symbol, env: Symbol, capCode: W, source: SourceInfo = UnknownSource) extends W(source) {
  override def value = ???
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
  val BoolEq, BoolNot, DictContains, DictGet, DictInsert, DictMake, DictRemove, DictSize, DictToList, Error, Eval, If, ListCons, ListHead, ListIsEmpty, ListMake, ListTail, NumAdd, NumDiv, NumEq, NumGT, NumGTE, NumLT, NumLTE, NumMult, NumSub, NumToCharList, Parse, Quote, ReadFile, SymEq, SymToCharList, Trace, TypeEq, TypeOf, Vau = Value
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
}
