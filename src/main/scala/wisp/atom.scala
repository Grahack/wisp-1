package wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

trait W {
  def verbose: String = summary
  def summary: String = name
  def name: String = this.getClass().toString()

  def execute(fn: WList): W = err
  def getAst: Stream[W] = err
  def hostBool: Boolean = err
  def hostChar: Char = err
  def hostDict: HashMap[W, W] = err
  def hostList: Stream[W] = err
  def hostNum: Int = err
  def hostSym: Symbol = err
  def hostType: WTypes.WType = err

  protected def err = sys.error("Operation not supported on: " + summary)
}

class WBool(value: Boolean) extends W {
  override def name = if (value) "True" else "False"
  override def hostBool = value
}

class WChar(val value: Char) extends W {
  override def name = "~" + value
  override def hostChar = value
  override def equals(o: Any) = o match {
    case o: WChar => value == o.value
    case i: Char => value == i
    case _ => false
  }
}

class WDict(val value: HashMap[W, W]) extends W {
  override def summary = "{Dict of " + value.size + "}"
  override def name = "Dict"
  override def hostDict = value
  override def equals(o: Any) =
    o match {
      case d: WDict => value == d.value
      case i: HashMap[_, _] => value == i
      case _ => false
    }
}

object FnArgs {
  def unapplySeq(w: WList) = {
    assert(!w.value.isEmpty)
    Stream.unapplySeq(w.value.tail)
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

object WIf extends W {
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

trait WFunc extends W {
  override def execute(fn: WList) = fn.value match {
    case Stream(_, a) => execute1(fn, a)
    case Stream(_, a, b) => execute2(fn, a, b)
    case Stream(_, a, b, c) => execute3(fn, a, b, c)
  }
  def execute1(fn: WList, a: W): W = err
  def execute2(fn: WList, a: W, b: W): W = err
  def execute3(fn: WList, a: W, b: W, c: W): W = err
}

trait WLambda extends W {
  override def name = "lambda"
  //  override def execute(fn: WList) = {
  //    FnCall(argS: WSym, code: W) =>
  //              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
  //              WLambdaRun(e, argS, code)
  //          }
}

trait AstOf extends WFunc {
  override def execute1(fn: WList, a: W) = new WList(a.getAst) with DerivedFrom { def from = fn }
}

trait TypeEq extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WBool(a.hostType == b.hostType) with DerivedFrom { def from = fn }
}

trait TypeOf extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WType(a.hostType) with DerivedFrom { def from = fn } // TODO: this is totally wrong
}

// boolean

trait BoolNot extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a) => new WBool(!a.hostBool) with DerivedFrom { def from = fn } }
}

trait BoolEq extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a, b) => new WBool(a.hostBool == a.hostBool) with DerivedFrom { def from = fn } }
}

// num

trait NumAdd extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a, b) => new WNum(a.hostNum + b.hostNum) with DerivedFrom { def from = fn } }
}

trait NumDiv extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WNum(a.hostNum / b.hostNum) with DerivedFrom { def from = fn }
}

trait NumGT extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WBool(a.hostNum > b.hostNum) with DerivedFrom { def from = fn }
}

trait NumGTE extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WBool(a.hostNum >= b.hostNum) with DerivedFrom { def from = fn }
}

trait NumEq extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WBool(a.hostNum == b.hostNum) with DerivedFrom { def from = fn }
}

trait NumLT extends WFunc {
  override def execute2(fn: WList, a: W, b: W) = new WBool(a.hostNum < b.hostNum) with DerivedFrom { def from = fn }
}

trait NumLTE extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a, b) => new WBool(a.hostNum <= b.hostNum) with DerivedFrom { def from = fn } }
}

trait NumMult extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a, b) => new WNum(a.hostNum * b.hostNum) with DerivedFrom { def from = fn } }
}

trait NumSub extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a, b) => new WNum(a.hostNum - b.hostNum) with DerivedFrom { def from = fn } }
}

trait NumToList extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a) => new WList(a.hostNum.toString.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn } }
}

// sym stuff

trait SymEq extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(a, b) => new WBool(a.hostSym == b.hostSym) with DerivedFrom { def from = fn } }
}

trait SymToList extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(list) => new WList(list.hostSym.name.map(x => new WChar(x)).toStream) with DerivedFrom { def from = fn } }
}

// list stuff

trait ListCons extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(list, value) => new WList(value #:: list.hostList) with DerivedFrom { def from = fn } }
}

trait ListHead extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(list) => list.hostList.head }
}

trait ListIsEmpty extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(list) => new WBool(list.hostList.isEmpty) with DerivedFrom { def from = fn } }
}

// TODO: this is for convenience, remove later
trait ListLength extends WFunc {
  override def execute(fn: WList) = fn match { case FnArgs(list) => new WNum(list.hostList.length) with DerivedFrom { def from = fn } }
}

trait ListMake extends WFunc {
  override def execute(fn: WList) = new WList(fn.value.tail) with DerivedFrom { def from = fn } // the head is #ListMake part
  override def equals(o: Any) = o.isInstanceOf[ListMake] // testing hack
}

// TODO: this should be removed, it's just for convenience
trait ListNth extends WFunc {
  override def execute2(fn: WList, list: W, i: W) = list.hostList(i.hostNum)
}

trait ListTail extends WFunc {
  override def execute1(fn: WList, list: W) = new WList(list.hostList.tail) with DerivedFrom { def from = fn }
}

trait DictContains extends WFunc {
  override def execute2(fn: WList, dict: W, key: W) = new WBool(dict.hostDict.contains(key)) with DerivedFrom { def from = fn }
}

trait DictGet extends WFunc {
  override def execute2(fn: WList, dict: W, key: W) = dict.hostDict(key)
}

trait DictInsert extends WFunc {
  override def execute3(fn: WList, dict: W, key: W, value: W) = {
    val d = dict.hostDict
    require(!d.contains(key))
    new WDict(d + (key -> value)) with DerivedFrom { def from = fn }
  }
}

trait DictRemove extends WFunc {
  override def execute2(fn: WList, dict: W, key: W) = {
    val d = dict.hostDict
    require(d.contains(key))
    new WDict(d - key) with DerivedFrom { def from = fn }
  }
}

trait DictSize extends WFunc {
  override def execute1(fn: WList, dict: W) = new WNum(dict.hostDict.size) with DerivedFrom { def from = fn }
}

trait DictToList extends WFunc {
  override def execute1(fn: WList, dict: W) = new WList(dict.hostDict.toStream.map(x => new WList(Stream(x._1, x._2)))) with DerivedFrom { def from = fn }
}

trait Trace extends WFunc {
  override def execute(fn: WList) = {
    println("Tracing: " + fn.summary)
    new WList(Stream()) with DerivedFrom { def from = fn }
  }
}

trait Error extends WFunc {
  override def execute(fn: WList) = sys.error("Code Error." + fn.summary) // TODO: better info
}