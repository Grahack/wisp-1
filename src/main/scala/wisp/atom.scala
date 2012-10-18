package wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

trait W {
  def verbose: String = summary
  def summary: String = name
  def name: String = this.getClass().toString()
  
  def getEnv: HashMap[W, W] = err("getEnv")

  def hostBool: Boolean = err("hostBool")
  def hostChar: Char = err("hostChar")
  def hostDict: HashMap[W, W] = err("hostDict")
  def hostList: Stream[W] = err("hostList")
  def hostNum: Int = err("hostNum")
  def hostSym: Symbol = err("hostSym")
  def hostType: WTypes.WType = err("hostType")
  def hostVect: IndexedSeq[W] = err("hostVect")

  def execute(fn: WList, env: HashMap[W, W]): W = err("execute")

  protected def err(op: String) = sys.error("Operation " + op + " not supported on: " + verbose)

  override def toString = summary
  override def hashCode = name.hashCode()
}

class Bool(val value: Boolean) extends W {
  override def name = if (value) "True" else "False"
  override def hostBool = value
  override def equals(o: Any) = o match {
    case wb: Bool => value == wb.value
    case b: Boolean => value == b
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class WChar(val value: Char) extends W {
  override def name = "~" + value
  override def hostChar = value
  override def equals(o: Any) = o match {
    case wc: WChar => value == wc.value
    case c: Char => value == c
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class Dict(val value: HashMap[W, W]) extends W {
  override def name = "Dict"
  override def summary =
    "{" + value.toList.map(x => (x._1.verbose + " " + x._2.verbose)).mkString(", ") + "}"
  override def hostDict = value
  override def equals(o: Any) = o match {
    case d: Dict => value == d.value
    case i: HashMap[_, _] => value == i
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class WList(val value: Stream[W]) extends W {
  override def name = "List"
  override def toString = "(List: " + value.toString + ")"
  override def hostList = value
  override def equals(o: Any) = o match {
    case l: WList => value == l.value
    case s: Seq[_] => value == s
    case _ => false
  }
  
  def evaledArgs(e: HashMap[W,W]) = value.tail.map(Interpretter.eval(e, _))

  override def verbose = "(" + value.map(_.verbose).mkString(" ") + ")"
  override def hashCode = value.hashCode()
}

class Vect(val value: IndexedSeq[W]) extends W {
  override def name = "Vect of " + value.length
  override def summary = if (value.forall { _.isInstanceOf[WChar] }) '"' + value.map(_.asInstanceOf[WChar].value).mkString + '"' else name
  override def hostVect = value
  override def equals(o: Any) =
    o match {
      case l: Vect => value == l.value
      case s: Seq[_] => value == s
      case st: String => asString.map(_ == st).getOrElse(false)
      case _ => false
    }

  private def asString: Option[String] = {
    val builder = StringBuilder.newBuilder

    value.foreach { x =>
      if (x.isInstanceOf[WChar])
        builder += x.asInstanceOf[WChar].value
      else
        return None
    }

    Some(builder.result)
  }

  override def verbose = "[" + value.map(_.verbose).mkString(" ") + "]"
  override def hashCode = asString.map(_.hashCode).getOrElse(value.hashCode)
}

class Num(val value: Int) extends W {
  override def name = value.toString()
  override def hostNum = value
  override def equals(o: Any) = o match {
    case n: Num => value == n.value
    case i: Int => value == i
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class Sym(val value: Symbol) extends W {
  override def name = value.name.toString()
  override def verbose = ":" + value.name
  override def hostSym = value
  override def equals(o: Any) =
    o match {
      case as: Sym => value == as.value
      case s: Symbol => value == s
      case _ => false
    }
  override def hashCode = value.hashCode()
}

trait If extends W {
  override def name = "If"
  // implementation in Interpretter, for tail-calls
}

// primitive

trait Eval extends W {
  override def name = "Eval"
  // implementation in Interpretter, for tail-calls
}

case class VauRun(capEnv: HashMap[W, W], arg: Sym, env: Sym, capCode: W) extends W {
  override def name = "$UDF$"
  // implementation in Interpretter, for tail-calls
}

object WTypes extends Enumeration {
  type WType = Value
  val TypeBool, TypeSym, TypeNum, TypeDict, TypeFunc, TypeList, TypeType = Value
}

class WType(val value: WTypes.WType) extends W {
  override def name = "{Type: " + value.toString + "}"
  override def hostType = value
  override def equals(o: Any) =
    o match {
      case at: WType => value == at.value
      case t: WTypes.WType => value == t
      case _ => false
    }
}

trait DerivedFrom {
  def from: W
}

trait Vau extends W {
  override def name = "Vau"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(argSym, envSym, code) = fn.evaledArgs(env)
    require(argSym.isInstanceOf[Sym], "Vau expects that the arg is a symbol, got: " + argSym)
    require(argSym == Symbol("_") || !env.contains(argSym), "The environment already contains arg symbol: " + argSym)

    require(envSym.isInstanceOf[Sym], "Vau expects that the env is a symbol, got: " + argSym)
    require(envSym == Symbol("_") || !env.contains(envSym), "The environment already contains env symbol: " + argSym)

    require(envSym == Symbol("_") || argSym != envSym, "ArgSymbol and EnvSymbol: " + argSym + " must not be the same")
    
    new VauRun(env, argSym.asInstanceOf[Sym], envSym.asInstanceOf[Sym], code)
  }
}

trait Quote extends W {
  override def name = "Quote"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    require(fn.value.length == 2, "Argument to quote must have exactly 1 argument")
    fn.value(1)
  }
  override def equals(o: Any) = o.isInstanceOf[Quote]
}

trait TypeEq extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostType == b.hostType) with DerivedFrom { def from = fn }
  }
}

trait TypeOf extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a) = fn.evaledArgs(env)
    new WType(a.hostType) with DerivedFrom { def from = fn } // TODO: this is totally wrong
  }
}

// boolean

trait BoolNot extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a) = fn.evaledArgs(env)
    new Bool(!a.hostBool) with DerivedFrom { def from = fn }
  }
}

trait BoolEq extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostBool == b.hostBool) with DerivedFrom { def from = fn }
  }
}

// num

trait NumAdd extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostNum + b.hostNum) with DerivedFrom { def from = fn }
  }
  override def name = "NumAdd"
}

trait NumDiv extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostNum / b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumGT extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostNum > b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumGTE extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostNum >= b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumEq extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostNum == b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumLT extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostNum < b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumLTE extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostNum <= b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumMult extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostNum * b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumSub extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Num(a.hostNum - b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumToCharList extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a) = fn.evaledArgs(env)
    new WList(a.hostNum.toString.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// sym stuff

trait SymEq extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(a, b) = fn.evaledArgs(env)
    new Bool(a.hostSym == b.hostSym) with DerivedFrom { def from = fn }
  }
}

trait SymToCharList extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new WList(list.hostSym.name.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// list stuff

trait ListCons extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list, value) = fn.evaledArgs(env)
    new WList(value #:: list.hostList) with DerivedFrom { def from = fn }
  }
}

trait ListHead extends W {
  override def name = "ListHead"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    list.hostList.head
  }
}

trait ListIsEmpty extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new Bool(list.hostList.isEmpty) with DerivedFrom { def from = fn }
  }
}

trait ListMake extends W {
  override def execute(fn: WList, env: HashMap[W, W]) =
    new WList(fn.evaledArgs(env)) with DerivedFrom { def from = fn }
}

trait ListTail extends W {
  override def name = "ListTail"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new WList(list.hostList.tail) with DerivedFrom { def from = fn }
  }
}

trait VectAppend extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list, value) = fn.evaledArgs(env)
    new Vect(list.hostVect :+ value) with DerivedFrom { def from = fn }
  }
}

trait VectLength extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(list) = fn.evaledArgs(env)
    new Num(list.hostVect.length) with DerivedFrom { def from = fn }
  }
}

trait VectMake extends W {
  override def execute(fn: WList, env: HashMap[W, W]) =
    new Vect(fn.evaledArgs(env).toIndexedSeq) with DerivedFrom { def from = fn }
}

trait VectNth extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(vect, index) = fn.evaledArgs(env)
    vect.hostVect(index.hostNum)
  }
}

trait VectPrepend extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(vect, value) = fn.evaledArgs(env)
    new Vect(value +: vect.hostVect) with DerivedFrom { def from = fn }
  }
}

trait VectToList extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(vect) = fn.evaledArgs(env)
    new WList(vect.hostVect.toStream) with DerivedFrom { def from = fn }
  }
}

trait DictContains extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict, key) = fn.evaledArgs(env)
    new Bool(dict.hostDict.contains(key)) with DerivedFrom { def from = fn }
  }
}

trait DictGet extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict, key) = fn.evaledArgs(env)
    dict.hostDict(key)
  }
}

trait DictInsert extends W {
  override def execute(fn: WList, env: HashMap[W, W]): W = {
    val Stream(dict: W, key: W, value: W) = fn.evaledArgs(env)
    val d = dict.hostDict
    require(!d.contains(key))
    new Dict(d + (key -> value)) with DerivedFrom { def from = fn }
  }
}

trait DictRemove extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict, key) = fn.evaledArgs(env)
    val d = dict.hostDict
    require(d.contains(key))
    new Dict(d - key) with DerivedFrom { def from = fn }
  }
}

trait DictSize extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict) = fn.evaledArgs(env)
    new Num(dict.hostDict.size) with DerivedFrom { def from = fn }
  }
}

trait DictToList extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(dict) = fn.evaledArgs(env)
    new WList(dict.hostDict.toStream.map(x => new WList(Stream(x._1, x._2)))) with DerivedFrom { def from = fn }
  }
}

trait Trace extends W {
  override def execute(fn: WList, env: HashMap[W, W]) = {
    println("Tracing: " + fn.evaledArgs(env).map(_.verbose).mkString(" "))
    new WList(Stream()) with DerivedFrom { def from = fn }
  }
}

trait WError extends W {
  override def execute(fn: WList, env: HashMap[W, W]) =
    sys.error("Code Error." + fn.evaledArgs(env).map(_.summary).mkString(", ")) // TODO: better info
}
