package wisp

import scala.collection.immutable.HashMap

trait W {
  def summary: String

  def rawBool: Boolean = err
  def rawChar: Char = err
  def rawDict: HashMap[W, W] = err
  def rawList: Stream[W] = err
  def rawAst: WList = err
  def rawNum: Int = err
  def rawSym: Symbol = err
  def rawType: WTypes.WType = err

  def err = sys.error("Operation not supported on: " + summary)
}

object WFalse extends W {
  def summary = "#False"
  override def rawBool = false
  def value = false
  override def equals(o: Any) = {
    o match {
      case WFalse => true
      case false => true
      case _ => false
    }
  }
}

object WTrue extends W {
  def summary = "#True"
  override def rawBool = true
  def value = true
  override def equals(o: Any) = {
    o match {
      case WTrue => true
      case true => true
      case _ => false
    }
  }
}

object WBool {
  def apply(value: Boolean) = if (value) WTrue else WFalse
}

class WChar(val value: Char) extends W {
  def summary = "~" + value
  override def rawChar = value
  override def equals(o: Any) = {
    o match {
      case o: WChar => value == o.value
      case i: Char => value == i
      case _ => false
    }
  }
}

class WDict(val value: HashMap[W, W]) extends W {
  def summary = "{ map of size: " + value.size + "}"
  override def rawDict = value
  override def equals(o: Any) =
    o match {
      case l: WDict => value == l.value
      case i: HashMap[_, _] => value == i
      case _ => false
    }
}

// TODO: Once everything is working and tested, we should be able to make a significantly more efficient version of stream
class WList(val value: Stream[W]) extends W {
  def summary = value.toString() // TODO: truncate if > a certain amount
  override def rawList = value
  override def equals(o: Any) =
    o match {
      case l: WList => value == l.value
      case s: Seq[_] => value == s
      case _ => false
    }
}

// TODO: don't use a string for debug info, lol
class WParamList(debugInfo: String, env: HashMap[W,W], unevaldArgs: Stream[W]) extends W {
  def summary = "{PL: " + debugInfo + " preeval: " + unevaldArgs + "}"
  override def rawList = unevaldArgs.map(Interpretter.eval(_, env))
  override def rawAst = new WList(unevaldArgs)
}

class WNum(val value: Int) extends W {
  def summary = value.toString()
  override def rawNum = value
  override def equals(o: Any) = o match {
    case n: WNum => value == n.value
    case i: Int => value == i
    case _ => false
  }
}

class WSym(val value: Symbol) extends W {
  def summary = value.toString() // TODO: truncate if > a certain amount
  override def rawSym = value
  override def equals(o: Any) =
    o match {
      case as: WSym => value == as.value
      case s: Symbol => value == s
      case _ => false
    }
}

// primitive

object WEval extends W {
  def summary = "#eval"
}

object WIf extends W {
  def summary = "#if"
}

object WLambda extends W {
  def summary = "#vau"
}

case class WLambdaRun(capEnv: HashMap[W,W], argS: WSym, capCode: W) extends W {
  override def summary = "$vau$"
}

object WTypes extends Enumeration {
  type WType = Value
  val TypeBool, TypeSym, TypeNum, TypeDict, TypeFunc, TypeList, TypeType = Value
}

class WType(val value: WTypes.WType) extends W {
  def summary = "Type: " + value.toString
  override def rawType = value
  override def equals(o: Any) =
    o match {
      case at: WType => value == at.value
      case t: WTypes.WType => value == t
      case _ => false
    }
}

object WFunc {
  var subClasses: Set[WFunc] = Set()
}

abstract class WFunc extends W {
  register()

  def summary = name.toString
  def name: Symbol
  def value = this
  
  def apply(s: Stream[W]): W = {
    require(run.isDefinedAt(s), "Function: " + summary + " couldn't accept the arguments: " + s)
    run(s)
  }

  protected def run: PartialFunction[Stream[W], W]

  protected def register() { WFunc.subClasses = WFunc.subClasses + this } // this is stupidly hacky...
}

object AstOf extends WFunc {
  def name = Symbol("#ast-of")
  def run = { case Stream(a) => a.rawAst }
}

object TypeEq extends WFunc {
  def name = Symbol("#TypeEq")
  def run = { case Stream(a, b) => WBool(a.rawType == b.rawType) }
}

object TypeOf extends WFunc {
  def name = Symbol("#TypeOf")
  def run = { case Stream(a) => new WType(a.rawType) } // TODO: this is totally wrong
}

// boolean

object BoolNot extends WFunc {
  def name = Symbol("#bool-not")
  def run = { case Stream(a) => WBool(!a.rawBool) }
}

object BoolEq extends WFunc {
  def name = Symbol("#bool-eq")
  def run = { case Stream(a, b) => WBool(a.rawBool == a.rawBool) }
}

// num

object NumAdd extends WFunc {
  def name = Symbol("#num-add")
  def run = { case Stream(a, b) => new WNum(a.rawNum + b.rawNum) }
}
object NumDiv extends WFunc {
  def name = Symbol("#num-div")
  def run = { case Stream(a, b) => new WNum(a.rawNum / b.rawNum) }
}
object NumGT extends WFunc {
  def name = Symbol("#num-gt")
  def run = { case Stream(a, b) => WBool(a.rawNum > b.rawNum) }
}
object NumGTE extends WFunc {
  def name = Symbol("#num-gte")
  def run = { case Stream(a, b) => WBool(a.rawNum >= b.rawNum) }
}
object NumEq extends WFunc {
  def name = Symbol("num-eq")
  def run = { case Stream(a, b) => WBool(a.rawNum == b.rawNum) }
}
object NumLT extends WFunc {
  def name = Symbol("#num-lt")
  def run = { case Stream(a, b) => WBool(a.rawNum < b.rawNum) }
}
object NumLTE extends WFunc {
  def name = Symbol("#num-lt")
  def run = { case Stream(a, b) => WBool(a.rawNum <= b.rawNum) }
}
object NumMult extends WFunc {
  def name = Symbol("#num-mult")
  def run = { case Stream(a, b) => new WNum(a.rawNum * b.rawNum) }
}
object NumSub extends WFunc {
  def name = Symbol("#num-sub")
  def run = { case Stream(a, b) => new WNum(a.rawNum - b.rawNum) }
}
object NumToList extends WFunc {
  def name = Symbol("#num-to-list")
  def run = { case Stream(a) => new WList(a.rawNum.toString.map(x => new WChar(x)).toStream) }
}

// sym stuff

object SymEq extends WFunc {
  def name = Symbol("#sym-eq")
  def run = { case Stream(a, b) => WBool(a.rawSym == b.rawSym) }
}

object SymToList extends WFunc {
  def name = Symbol("#sym-to-list")
  def run = { case Stream(list) => new WList(list.rawSym.name.map(x => new WChar(x)).toStream) }
}

// list stuff

object ListCons extends WFunc {
  def name = Symbol("#list-cons")
  def run = { case Stream(list, value) => new WList(value #:: list.rawList) }
}

object ListIsEmpty extends WFunc {
  def name = Symbol("#list-empty?")
  def run = { case Stream(list) => WBool(list.rawList.isEmpty) }
}

// TODO: this is for convenience, remove later
object ListLength extends WFunc {
  def name = Symbol("#list-length")
  def run = { case Stream(list) => new WNum(list.rawList.length) }
}

object ListMake extends WFunc {
  def name = Symbol("#list-make")
  def run = { case list => new WList(list) }
}

// TODO: this should be removed, it's just for convenience
object ListNth extends WFunc {
  def name = Symbol("#list-nth")
  def run = { case Stream(list, i) => list.rawList(i.rawNum) }
}

object ListTail extends WFunc {
  def name = Symbol("#list-tail")
  def run = { case list => new WList(list.tail) }
}

object DictContains extends WFunc {
  def name = Symbol("#dict-contains")
  def run = { case Stream(dict, key) => WBool(dict.rawDict.contains(key)) }
}

object DictGet extends WFunc {
  def name = Symbol("#dict-get")
  def run = { case Stream(dict, key) => dict.rawDict(key) }
}

object DictInsert extends WFunc {
  def name = Symbol("#dict-insert")
  def run = {
    case Stream(dict, key, value) =>
      val d = dict.rawDict
      require(!d.contains(key))
      new WDict(d + (key -> value))
  }
}

object DictRemove extends WFunc {
  def name = Symbol("#dict-remove")
  def run = {
    case Stream(dict, key) =>
      val d = dict.rawDict
      require(d.contains(key))
      new WDict(d - key)
  }
}

object DictSize extends WFunc {
  def name = Symbol("#dict-size")
  def run = { case Stream(dict) => new WNum(dict.rawDict.size) }
}

object DictToList extends WFunc {
  def name = Symbol("#dict-to-list")
  def run = { case Stream(dict) => new WList(dict.rawDict.toStream.map(x => new WList(Stream(x._1, x._2)))) }
}

object Trace extends WFunc {
  def name = Symbol("#trace")
  def run = {
    case x =>
      import scala.util.parsing.input.Positional
      println("Tracing: {" + x.map(_.summary).mkString(";") + "}" + (if (x.isInstanceOf[Positional]) (" pos: " + x.asInstanceOf[Positional].pos.longString) else ""))
      new WList(Stream())
  }
}

object Error extends WFunc {
  def name = Symbol("#error")
  def run = { case _ => sys.error("Code Error") } // TODO: better info
}