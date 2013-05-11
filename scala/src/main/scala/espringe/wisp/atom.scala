package espringe.wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

sealed trait SourceInfo {
  def print: String
}
object UnknownSource extends SourceInfo {
  def print = "Source: Unknown"
}
class LexicalSource(file: String, pos: Long) extends SourceInfo {
  def print = s"Source: $file position $pos"
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
  def asSym: Option[Sym] = None
  def asType: Option[Primitives.Primitive] = None
  def asStream: Option[Stream[W]] = None

  override def hashCode: Int = ???
  override def toString = deparse
}

case class Bool(value: Boolean, source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = if (value) "True" else "False"
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
/*
// num
trait NumAdd extends W {
  override def toString = "NumAdd"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostInt + b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumDiv extends W {
  override def toString = "NumDiv"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostInt / b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumGT extends W {
  override def toString = "NumGT"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostInt > b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumGTE extends W {
  override def toString = "NumGTE"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostInt >= b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumEq extends W {
  override def toString = "NumEq"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostInt == b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumLT extends W {
  override def toString = "NumLT"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostInt < b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumLTE extends W {
  override def toString = "NumLTE"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostInt <= b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumMult extends W {
  override def toString = "NumMult"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostInt * b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumSub extends W {
  override def toString = "NumSub"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostInt - b.hostInt) with DerivedFrom { def from = fn }
  }
}

trait NumToCharList extends W {
  override def toString = "NumToCharList"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a) = fn.evaledArgs(env)
    new WList(a.hostInt.toString.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// sym stuff

trait SymEq extends W {
  override def toString = "SymEq"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostSym == b.hostSym) with DerivedFrom { def from = fn }
  }
}

trait SymToCharList extends W {
  override def toString = "SymToCharList"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new WList(list.hostSym.name.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// list stuff

trait ListCons extends W {
  override def toString = "ListCons"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list, value) = fn.evaledArgs(env)
    new WList(value #:: list.hostStream) with DerivedFrom { def from = fn }
  }
}

trait ListHead extends W {
  override def toString = "ListHead"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    list.hostStream.head
  }
}

trait ListIsEmpty extends W {
  override def toString = "ListIsEmpty"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new Bool(list.hostStream.isEmpty) with DerivedFrom { def from = fn }
  }
  override def equals(o: Any) = o.isInstanceOf[ListIsEmpty]
}

 */
class ListMake(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#list-make"
  override def typeOf = Primitives.TypeBuiltIn
}

/*
trait ListTail extends W {
  override def toString = "ListTail"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new WList(list.hostStream.tail) with DerivedFrom { def from = fn }
  }
  override def equals(o: Any) = o.isInstanceOf[ListTail]
}

trait DictContains extends W {
  override def toString = "DictContains"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict, key) = fn.evaledArgs(env)
    new Bool(dict.hostHashMap.contains(key)) with DerivedFrom { def from = fn }
  }
}

trait DictGet extends W {
  override def toString = "DictGet"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict, key) = fn.evaledArgs(env)
    dict.hostHashMap(key)
  }
}

trait DictInsert extends W {
  override def toString = "DictInsert"
  override def execute(fn: WList, env: HashMap[W, W]): W = {
    val Stream(dict: W, key: W, value: W) = fn.evaledArgs(env)
    val d = dict.hostHashMap
    require(!d.contains(key))
    new Dict(d + (key -> value)) with DerivedFrom { def from = fn }
  }
}

trait DictRemove extends W {
  override def toString = "DictRemove"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict, key) = fn.evaledArgs(env)
    val d = dict.hostHashMap
    require(d.contains(key))
    new Dict(d - key) with DerivedFrom { def from = fn }
  }
}

trait DictSize extends W {
  override def toString = "DictSize"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict) = fn.evaledArgs(env)
    new Num(dict.hostHashMap.size) with DerivedFrom { def from = fn }
  }
}

trait DictToList extends W {
  override def toString = "DictToList"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict) = fn.evaledArgs(env)
    new WList(dict.hostHashMap.toStream.map(x => new WList(Stream(x._1, x._2)))) with DerivedFrom { def from = fn }
  }
}
*/

class DictMake(source: SourceInfo = UnknownSource) extends W(source) {
  override def deparse = "#dict-make"
  override def typeOf = Primitives.TypeBuiltIn
}

/*
trait Trace extends W {
  override def toString = "Trace"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val args = fn.evaledArgs(env)
    require(args.length >= 1)
    println("Tracing: " + args.mkString(" "))

    args.last
  }
}

trait WError extends W {
  override def toString = "Error"
  override def execute(fn: WList, env: HashMap[W, W]) =
    sys.error("Code Error." + fn.evaledArgs(env).mkString(", ")) // TODO: better info
}
* 
*/

