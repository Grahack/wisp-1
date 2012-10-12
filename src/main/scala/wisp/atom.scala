package wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

trait W {
  def verbose: String = summary
  def summary: String = name
  def name: String = this.getClass().toString()

  def getAst: Stream[W] = err
  def hostBool: Boolean = err
  def hostChar: Char = err
  def hostDict: HashMap[W, W] = err
  def hostList: Stream[W] = err
  def hostNum: Int = err
  def hostSym: Symbol = err
  def hostType: WTypes.WType = err

  def execute(fn: WList, args: Stream[W]): W = err

  protected def err = sys.error("Operation not supported on: " + summary)

  override def toString = summary
}

class WBool(val value: Boolean) extends W {
  override def name = if (value) "True" else "False"
  override def hostBool = value
  override def equals(o: Any) = o match {
    case wb: WBool => value == wb.value
    case b: Boolean => value == b
    case _ => false
  }
}

class WChar(val value: Char) extends W {
  override def name = "~" + value
  override def hostChar = value
  override def equals(o: Any) = o match {
    case wc: WChar => value == wc.value
    case c: Char => value == c
    case _ => false
  }
}

class WDict(val value: HashMap[W, W]) extends W {
  override def summary = "{Dict of " + value.size + "}"
  override def name = "Dict"
  override def hostDict = value
  override def equals(o: Any) = o match {
    case d: WDict => value == d.value
    case i: HashMap[_, _] => value == i
    case _ => false
  }
}

// TODO: Once everything is working and tested, we should be able to make a significantly more efficient version of stream
class WList(val value: Stream[W]) extends W {
  override def name = "List"
  override def hostList = value
  override def equals(o: Any) =
    o match {
      case l: WList => value == l.value
      case s: Seq[_] => value == s
      case st: String => value == st.toStream
      case _ => false
    }
}

// TODO: don't use a string for debug info, lol
class WParamList(debugInfo: String, env: HashMap[W, W], unevaldArgs: Stream[W]) extends W {
  override def summary = "{PL: " + debugInfo + " preeval: " + unevaldArgs + "}"
  override def name = "ParamList"
  override def getAst = unevaldArgs
  override def hostList = unevaldArgs.map(Interpretter.eval(_, env))
}

class WNum(val value: Int) extends W {
  override def name = value.toString()
  override def hostNum = value
  override def equals(o: Any) = o match {
    case n: WNum => value == n.value
    case i: Int => value == i
    case _ => false
  }
}

class WSym(val value: Symbol) extends W {
  override def name = value.toString()
  override def hostSym = value
  override def equals(o: Any) =
    o match {
      case as: WSym => value == as.value
      case s: Symbol => value == s
      case _ => false
    }
}

// primitive

trait WEval extends W {
  override def summary = "WEval"
}

trait WIf extends W {
  override def name = "if"
}

case class WLambdaRun(capEnv: HashMap[W, W], argS: WSym, capCode: W) extends W {
  override def name = "$UDF$"
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

trait WLambda extends W {
  override def name = "lambda"
  //  override def execute(fn: WList) = {
  //    FnCall(argS: WSym, code: W) =>
  //              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
  //              WLambdaRun(e, argS, code)
  //          }
}

trait AstOf extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a) = args
    new WList(a.getAst) with DerivedFrom { def from = fn }
  }
}

trait TypeEq extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostType == b.hostType) with DerivedFrom { def from = fn }
  }
}

trait TypeOf extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a) = args
    new WType(a.hostType) with DerivedFrom { def from = fn } // TODO: this is totally wrong
  }
}

// boolean

trait BoolNot extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a) = args
    new WBool(!a.hostBool) with DerivedFrom { def from = fn }
  }
}

trait BoolEq extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostBool == b.hostBool) with DerivedFrom { def from = fn }
  }
}

// num

trait NumAdd extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WNum(a.hostNum + b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumDiv extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WNum(a.hostNum / b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumGT extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostNum > b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumGTE extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostNum >= b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumEq extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostNum == b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumLT extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostNum < b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumLTE extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostNum <= b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumMult extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WNum(a.hostNum * b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumSub extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WNum(a.hostNum - b.hostNum) with DerivedFrom { def from = fn }
  }
}

trait NumToCharList extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a) = args
    new WList(a.hostNum.toString.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// sym stuff

trait SymEq extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(a, b) = args
    new WBool(a.hostSym == b.hostSym) with DerivedFrom { def from = fn }
  }
}

trait SymToCharList extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list) = args
    new WList(list.hostSym.name.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn }
  }
}

// list stuff

trait ListCons extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list, value) = args
    new WList(value #:: list.hostList) with DerivedFrom { def from = fn }
  }
}

trait ListHead extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list) = args
    list.hostList.head
  }
}

trait ListIsEmpty extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list) = args
    new WBool(list.hostList.isEmpty) with DerivedFrom { def from = fn }
  }
}

// TODO: this is for convenience, remove later
trait ListLength extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list) = args
    new WNum(list.hostList.length) with DerivedFrom { def from = fn }
  }
}

trait ListMake extends W {
  override def execute(fn: WList, args: Stream[W]) = new WList(args) with DerivedFrom { def from = fn }
  override def equals(o: Any) = o.isInstanceOf[ListMake] // testing hack
}

// TODO: this should be removed, it's just for convenience
trait ListNth extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list, index) = args
    list.hostList(index.hostNum)
  }
}

trait ListTail extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(list) = args
    new WList(list.hostList.tail) with DerivedFrom { def from = fn }
  }
}

trait DictContains extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(dict, key) = args
    new WBool(dict.hostDict.contains(key)) with DerivedFrom { def from = fn }
  }
}

trait DictGet extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(dict, key) = args
    dict.hostDict(key)
  }
}

trait DictInsert extends W {
  override def execute(fn: WList, args: Stream[W]): W = {
    val Stream(dict: W, key: W, value: W) = args
    val d = dict.hostDict
    require(!d.contains(key))
    new WDict(d + (key -> value)) with DerivedFrom { def from = fn }
  }
}

trait DictRemove extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(dict, key) = args
    val d = dict.hostDict
    require(d.contains(key))
    new WDict(d - key) with DerivedFrom { def from = fn }
  }
}

trait DictSize extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(dict) = args
    new WNum(dict.hostDict.size) with DerivedFrom { def from = fn }
  }
}

trait DictToList extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    val Stream(dict) = args
    new WList(dict.hostDict.toStream.map(x => new WList(Stream(x._1, x._2)))) with DerivedFrom { def from = fn }
  }
}

trait Trace extends W {
  override def execute(fn: WList, args: Stream[W]) = {
    println("Tracing: " + args.map(_.summary).mkString(", "))
    new WList(Stream()) with DerivedFrom { def from = fn }
  }
}

trait Error extends W {
  override def execute(fn: WList, args: Stream[W]) =
    sys.error("Code Error." + args.map(_.summary).mkString(", ")) // TODO: better info
}
