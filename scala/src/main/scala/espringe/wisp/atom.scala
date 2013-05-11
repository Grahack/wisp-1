package espringe.wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

sealed trait W {
  
  def deparse: String
  
  
  def asBool: Option[Boolean] = None
  def asDict: Option[Dict] = None

  override def hashCode: Int = ???
  override def toString = deparse
}

case class Bool(value: Boolean) extends W {
  override def deparse = if (value) "True" else "False"
}

case class WChar(value: Char) extends W {
  override def deparse = "~" + value
}

case class WDict(value: Dict) extends W {
  override def deparse =
    "{" + value.toList.map(x => (x._1.toString + " " + x._2.toString)).mkString(", ") + "}"
}

case class WList(value: Stream[W]) extends W {

  override def deparse = asString.map(x => s"'$x'")
    .getOrElse("(" + value.map(_.toString).mkString(" ") + ")")

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

case class Num(value: Long) extends W {
  override def deparse = value.toString
}


case class Sym(value: Symbol) extends W {
  override def deparse = value.name
}

case class If() extends W  {
  override def deparse = "#if"
}

case class Eval() extends W {
  override def deparse = "#eval"
}

case class ReadFile() extends W {
  override def deparse = "#read"
}

case class Parse() extends W {
  override def deparse = "#parse"
}

case class Vau() extends W {
  override def deparse = "#vau"
}


case class UDF(capEnv: Dict, arg: Sym, env: Sym, capCode: W) extends W {
  override def deparse = "#???UDF???"
}

object Primitives extends Enumeration {
  type Primitive = Value
  val TypeBool, TypeSym, TypeNum, TypeDict, TypeFunc, TypeList, TypeType = Value
}

case class WType(value: Primitives.Primitive) extends W {
  override def deparse = "{Type: " + value.toString + "}"
}

// The only non-strict builtin in wisp. Return the first argument unevaluated
case class Quote() extends W {
  override def deparse = "#quote"
}

/*
case class TypeEq() extends W {
  override def toString = "TypeEq"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostType == b.hostType) with DerivedFrom { def from = fn }
  }
}

trait TypeOf extends W {
  override def toString = "TypeOf"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a) = fn.evaledArgs(env)
    new WType(a.hostType) with DerivedFrom { def from = fn } // TODO: this is totally wrong
  }
}

// boolean
trait BoolNot extends W {
  override def toString = "BoolNot"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a) = fn.evaledArgs(env)
    new Bool(!a.hostBoolean) with DerivedFrom { def from = fn }
  }
}

trait BoolEq extends W {
  override def toString = "BoolEq"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostBoolean == b.hostBoolean) with DerivedFrom { def from = fn }
  }
}

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
case class ListMake() extends W {
  override def deparse = "#list-make"
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

case class DictMake() extends W {
  override def deparse = "#dict-make"
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

