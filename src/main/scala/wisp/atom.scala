package wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

trait W {
  def verbose: String = summary
  def summary: String = name
  def name: String = this.getClass().toString()

  def getAst: Stream[W] = err
  def getEnv: HashMap[W,W] = err
  
  def hostBool: Boolean = err
  def hostChar: Char = err
  def hostDict: HashMap[W, W] = err
  def hostList: Stream[W] = err
  def hostNum: Int = err
  def hostSym: Symbol = err
  def hostType: WTypes.WType = err
  def hostVect: IndexedSeq[W] = err

  def execute(fn: WList, args: => Stream[W]): W = err

  protected def err = sys.error("Operation not supported on: " + summary)

  override def toString = summary
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
  override def summary = "{Dict of " + value.size + "}"
  override def name = "Dict"
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

  override def verbose = "(" + value.map(_.verbose).mkString(" ") + ")"
  override def hashCode = value.hashCode()
}

class ParamList(env: HashMap[W, W], unevaldArgs: Stream[W]) extends W {
  override def summary = "{ParamList: " + unevaldArgs.map(_.summary).mkString(" ") + "}"
  override def name = "ParamList"
    
  override def getAst = unevaldArgs
  override def getEnv = env

  private lazy val memHostList = unevaldArgs.map(Interpretter.eval(_, env))

  override def hostList = memHostList
  override def hashCode = env.hashCode() ^ unevaldArgs.hashCode()
}

class Vect(val value: IndexedSeq[W]) extends W {
  override def name = "Vect of " + value.length
  override def summary = if (value.forall { _.isInstanceOf[WChar] }) '"' + value.map(_.asInstanceOf[WChar].value).mkString + '"' else name
  override def hostVect = value
  override def equals(o: Any) =
    o match {
      case l: Vect => value == l.value
      case s: Seq[_] => value == s
      case st: String => value == st.toSeq
      case _ => false
    }

  override def verbose = "[" + value.map(_.verbose).mkString(" ") + "]"
}

class Num(val value: Int) extends W {
  override def name = value.toString()
  override def hostNum = value
  override def equals(o: Any) = o match {
    case n: Num => value == n.value
    case i: Int => value == i
    case _ => false
  }
}

class Sym(val value: Symbol) extends W {
  override def name = value.toString()
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
  override def summary = "Eval"
  // implementation in Interpretter, for tail-calls
}

case class WLambdaRun(capEnv: HashMap[W, W], argS: Sym, capCode: W) extends W {
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

trait Lambda extends W {
  override def name = "Lambda"
  // impl in interpretter, since we don't have access to the environment
}

trait Quote extends W {
  override def name = "Quote"
  override def execute(fn: WList, args: => Stream[W]) = {
    require(fn.value.length == 2, "Argument to quote must have exactly 1 argument")
    fn.value(1)
  }
}

trait AstOf extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a) = args
    new WList(a.getAst) with DerivedFrom { def from = fn }
  }
}

trait EnvOf extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a) = args
    new Dict(a.getEnv) with DerivedFrom { def from = fn }
  }
}

trait TypeEq extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostType == b.hostType) with DerivedFrom { def from = fn }
  }
}

trait TypeOf extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a) = args
    new WType(a.hostType) with DerivedFrom { def from = fn } // TODO: this is totally wrong
  }
}

// boolean

trait BoolNot extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a) = args
    new Bool(!a.hostBool) with DerivedFrom { def from = fn }
  }
}

trait BoolEq extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostBool == b.hostBool) with DerivedFrom { def from = fn }
  }
}

// num

trait NumAdd extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Num(a.hostNum + b.hostNum) with DerivedFrom { def from = fn }
  }
  override def name = "NumAdd"
}

trait NumDiv extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Num(a.hostNum / b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumGT extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostNum > b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumGTE extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostNum >= b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumEq extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostNum == b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumLT extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostNum < b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumLTE extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostNum <= b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumMult extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Num(a.hostNum * b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumSub extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Num(a.hostNum - b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumToCharList extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a) = args
    new WList(a.hostNum.toString.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// sym stuff

trait SymEq extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(a, b) = args
    new Bool(a.hostSym == b.hostSym) with DerivedFrom { def from = fn }
  }
}

trait SymToCharList extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list) = args
    new WList(list.hostSym.name.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// list stuff

trait ListCons extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list, value) = args
    new WList(value #:: list.hostList) with DerivedFrom { def from = fn }
  }
}

trait ListHead extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list) = args
    list.hostList.head
  }
}

trait ListIsEmpty extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list) = args
    new Bool(list.hostList.isEmpty) with DerivedFrom { def from = fn }
  }
}

trait ListMake extends W {
  override def execute(fn: WList, args: => Stream[W]) =
    new WList(args) with DerivedFrom { def from = fn }
}

trait ListTail extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list) = args
    new WList(list.hostList.tail) with DerivedFrom { def from = fn }
  }
}

trait VectAppend extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list, value) = args
    new Vect(list.hostVect :+ value) with DerivedFrom { def from = fn }
  }
}

trait VectLength extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(list) = args
    new Num(list.hostVect.length) with DerivedFrom { def from = fn }
  }
}

trait VectMake extends W {
  override def execute(fn: WList, args: => Stream[W]) =
    new Vect(args.toIndexedSeq) with DerivedFrom { def from = fn }
}

trait VectNth extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(vect, index) = args
    vect.hostVect(index.hostNum)
  }
}

trait VectPrepend extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(vect, value) = args
    new Vect(value +: vect.hostVect) with DerivedFrom { def from = fn }
  }
}

trait VectToList extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(vect) = args
    new WList(vect.hostVect.toStream) with DerivedFrom { def from = fn }
  }
}

trait DictContains extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(dict, key) = args
    new Bool(dict.hostDict.contains(key)) with DerivedFrom { def from = fn }
  }
}

trait DictGet extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(dict, key) = args
    dict.hostDict(key)
  }
}

trait DictInsert extends W {
  override def execute(fn: WList, args: => Stream[W]): W = {
    val Stream(dict: W, key: W, value: W) = args
    val d = dict.hostDict
    require(!d.contains(key))
    new Dict(d + (key -> value)) with DerivedFrom { def from = fn }
  }
}

trait DictRemove extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(dict, key) = args
    val d = dict.hostDict
    require(d.contains(key))
    new Dict(d - key) with DerivedFrom { def from = fn }
  }
}

trait DictSize extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(dict) = args
    new Num(dict.hostDict.size) with DerivedFrom { def from = fn }
  }
}

trait DictToList extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    val Stream(dict) = args
    new WList(dict.hostDict.toStream.map(x => new WList(Stream(x._1, x._2)))) with DerivedFrom { def from = fn }
  }
}

trait Trace extends W {
  override def execute(fn: WList, args: => Stream[W]) = {
    println("Tracing: " + args.map(_.summary).mkString(" "))
    new WList(Stream()) with DerivedFrom { def from = fn }
  }
}

trait WError extends W {
  override def execute(fn: WList, args: => Stream[W]) =
    sys.error("Code Error." + args.map(_.summary).mkString(", ")) // TODO: better info
}
