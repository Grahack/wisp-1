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

class If(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#if"
  override def typeOf = Primitives.TypeBuiltIn
}

class Eval(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#eval"
  override def typeOf = Primitives.TypeBuiltIn
}

class ReadFile(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#read"
  override def typeOf = Primitives.TypeBuiltIn
}

class Parse(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#parse"
  override def typeOf = Primitives.TypeBuiltIn
}

class Vau(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#vau"
  override def typeOf = Primitives.TypeBuiltIn
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

class Deref(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#deref"
  override def typeOf = Primitives.TypeBuiltIn

}

class TypeEq(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#type-eq"
  override def typeOf = Primitives.TypeBuiltIn

}

class TypeOf(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#type-of"
  override def typeOf = Primitives.TypeBuiltIn
}

// boolean
class BoolNot(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#bool-not"
  override def typeOf = Primitives.TypeBuiltIn
}

class BoolEq(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#bool-eq"
  override def typeOf = Primitives.TypeBuiltIn
}

// list stuff

class ListCons(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#list-cons"
  override def typeOf = Primitives.TypeBuiltIn
}

class ListHead(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#list-head"
  override def typeOf = Primitives.TypeBuiltIn
}

class ListIsEmpty(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#list-empty?"
  override def typeOf = Primitives.TypeBuiltIn
}

class ListMake(source: SourceInfo = UnknownSource) extends W(source) {
  override def equals(o: Any) = o.isInstanceOf[ListMake]
  override def deparse = "#list-make"
  override def typeOf = Primitives.TypeBuiltIn
}

class ListTail(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#list-tail"
  override def typeOf = Primitives.TypeBuiltIn
}

// num
class NumAdd(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-add"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumDiv(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-div"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumGT(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-gt"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumGTE(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-gte"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumEq(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-eq"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumLT(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-lt"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumLTE(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-lte"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumMult(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-mult"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumSub(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-sub"
  override def typeOf = Primitives.TypeBuiltIn
}

class NumToCharList(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#num-to-char-list"
  override def typeOf = Primitives.TypeBuiltIn
}

// sym stuff

class SymEq(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#sym-eq"
  override def typeOf = Primitives.TypeBuiltIn
}

class SymToCharList(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#sym-to-char-list"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictContains(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-contains"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictGet(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-get"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictInsert(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-insert"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictRemove(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-remove"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictSize(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-size"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictToList(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-to-list"
  override def typeOf = Primitives.TypeBuiltIn
}

class DictMake(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-make"
  override def typeOf = Primitives.TypeBuiltIn
}

class Trace(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#trace"
  override def typeOf = Primitives.TypeBuiltIn
}

class WError(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#error"
  override def typeOf = Primitives.TypeBuiltIn
}
