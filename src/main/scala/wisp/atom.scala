package wisp

import scala.collection.immutable.HashMap

trait Atom {
  def summary: String

  def value: Any

  def rawBool: Boolean = err
  def rawChar: Char = err
  def rawDict: HashMap[Any, Any] = err
  def rawList: Stream[Any] = err
  def rawNum: Int = err
  def rawSym: Symbol = err
  def rawType: WTypes.WType = err

  def err = sys.error("Operation not supported on: " + summary)
}

object AtomFalse extends Atom {
  def summary = "#False"
  override def rawBool = false
  def value = false
  override def equals(o: Any) = {
    o match {
      case AtomFalse => true
      case false => true
      case _ => false
    }
  }
}

object AtomTrue extends Atom {
  def summary = "#True"
  override def rawBool = true
  def value = true
  override def equals(o: Any) = {
    o match {
      case AtomTrue => true
      case true => true
      case _ => false
    }
  }
}

object AtomBool {
  def apply(value: Boolean) = if (value) AtomTrue else AtomFalse
}

class AtomChar(val value: Char) extends Atom {
  def summary = "~" + value
  override def rawChar = value
  override def equals(o: Any) = {
    o match {
      case o: AtomChar => value == o.value
      case i: Char => value == i
      case _ => false
    }
  }
}

class AtomDict(val value: HashMap[Any, Any]) extends Atom {
  def summary = "{ map of size: " + value.size + "}"
  override def rawDict = value
  override def equals(o: Any) =
    o match {
      case l: AtomDict => value == l.value
      case i: HashMap[_, _] => value == i
      case _ => false
    }
}

class AtomList(val value: Stream[Atom]) extends Atom {
  def summary = value.toString() // TODO: truncate if > a certain amount
  override def rawList = value
  override def equals(o: Any) =
    o match {
      case l: AtomList => value == l.value
      case s: Seq[_] => value == s
      case _ => false
    }
}

class AtomNum(val value: Int) extends Atom {
  def summary = value.toString()
  override def rawNum = value
  override def equals(o: Any) = o match {
    case n: AtomNum => value == n.value
    case i: Int => value == i
    case _ => false
  }
}

class AtomSym(val value: Symbol) extends Atom {
  def summary = value.toString() // TODO: truncate if > a certain amount
  override def rawSym = value
  override def equals(o: Any) =
    o match {
      case as: AtomSym => value == as.value
      case s: Symbol => value == s
      case _ => false
    }
}

object WTypes extends Enumeration {
  type WType = Value
  val TypeBool, TypeSym, TypeNum, TypeDict, TypeFunc, TypeList, TypeType = Value
}

class AtomType(val value: WTypes.WType) extends Atom {
  def summary = "Type: " + value.toString
  override def rawType = value
  override def equals(o: Any) =
    o match {
      case at: AtomType => value == at.value
      case t: WTypes.WType => value == t
      case _ => false
    }
}

object AtomProc {
  var subClasses: Set[AtomProc] = Set()
}

abstract class AtomProc extends Atom {
  register()

  def summary = name.toString
  def name: Symbol
  def value = this

  protected def run: PartialFunction[Stream[Atom], Atom]

  protected def register() { AtomProc.subClasses = AtomProc.subClasses + this } // this is stupidly hacky...
}

object TypeEq extends AtomProc {
  def name = Symbol("#TypeEq")
  def run = { case Stream(a, b) => AtomBool(a.rawType == b.rawType) }
}

object TypeOf extends AtomProc {
  def name = Symbol("#TypeOf")
  def run = { case Stream(a) => new AtomType(a.rawType) }
}

// boolean

object BoolNot extends AtomProc {
  def name = Symbol("#bool-not")
  def run = { case Stream(a) => AtomBool(!a.rawBool) }
}

object BoolEq extends AtomProc {
  def name = Symbol("#bool-eq")
  def run = { case Stream(a, b) => AtomBool(a.rawBool == a.rawBool) }
}

// num

object NumAdd extends AtomProc {
  def name = Symbol("#num-add")
  def run = { case Stream(a, b) => new AtomNum(a.rawNum + b.rawNum) }
}
object NumDiv extends AtomProc {
  def name = Symbol("#num-div")
  def run = { case Stream(a, b) => new AtomNum(a.rawNum / b.rawNum) }
}
object NumGT extends AtomProc {
  def name = Symbol("#num-gt")
  def run = { case Stream(a, b) => AtomBool(a.rawNum > b.rawNum) }
}
object NumGTE extends AtomProc {
  def name = Symbol("#num-gte")
  def run = { case Stream(a, b) => AtomBool(a.rawNum >= b.rawNum) }
}
object NumEq extends AtomProc {
  def name = Symbol("num-eq")
  def run = { case Stream(a, b) => AtomBool(a.rawNum == b.rawNum) }
}
object NumLT extends AtomProc {
  def name = Symbol("#num-lt")
  def run = { case Stream(a, b) => AtomBool(a.rawNum < b.rawNum) }
}
object NumLTE extends AtomProc {
  def name = Symbol("#num-lt")
  def run = { case Stream(a, b) => AtomBool(a.rawNum <= b.rawNum) }
}
object NumMult extends AtomProc {
  def name = Symbol("#num-mult")
  def run = { case Stream(a, b) => new AtomNum(a.rawNum * b.rawNum) }
}
object NumSub extends AtomProc {
  def name = Symbol("#num-sub")
  def run = { case Stream(a, b) => new AtomNum(a.rawNum - b.rawNum) }
}
object NumToList extends AtomProc {
  def name = Symbol("#num-to-list")
  def run = { case Stream(a) => new AtomList(a.rawNum.toString.map(x => new AtomChar(x)).toStream) }
}

object ListMake extends AtomProc {
  def name = Symbol("#list-make")
  def run = { case a => new AtomList(a) }
}

object WFunc extends Enumeration {
  type WFunc = Value

  val Eval = Value // primitive (ish)
  val TypeEq, TypeOf = Value
  val BoolNot, BoolEq = Value
  val NumAdd, NumDiv, NumGreaterThan, NumGreaterThanOrEqual, NumEq, NumNeq, NumLessThan, NumLessThanOrEqual, NumMult, NumSub, NumToList = Value
  val SymEq, SymToVect = Value
  val ListCons, ListEmpty, ListLength, ListMake, ListRest, ListNth, ListReduce = Value // TODO: should only have: cons, rest, empty
  val DictContains, DictGet, DictInsert, DictRemove, DictSize, DictToList = Value
  val Trace, Error = Value // debuggy
}

