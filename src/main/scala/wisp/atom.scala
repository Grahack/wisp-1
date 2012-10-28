package wisp

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

trait W {
  def getEnv: HashMap[W, W] = err("getEnv")

  def hostBoolean: Boolean = err("hostBool")
  def hostChar: Char = err("hostChar")
  def hostHashMap: HashMap[W, W] = err("hostDict")
  def hostStream: Stream[W] = err("hostList")
  def hostInt: Int = err("hostNum")
  def hostSym: Symbol = err("hostSym")
  def hostType: WTypes.WType = err("hostType")
  def hostVect: IndexedSeq[W] = err("hostVect")
  def hostString: String = err("hostString")

  def execute(fn: WList, env: HashMap[W, W]): W = err("execute")

  protected def err(op: String) = sys.error("Operation " + op + " not supported on: " + this)

  override def equals(o: Any): Boolean = sys.error("Not implemented")

  override def hashCode: Int = toString.hashCode()
}

class Bool(val value: Boolean) extends W {
  override def toString = if (value) "True" else "False"
  override def hostBoolean = value
  override def equals(o: Any) = o match {
    case wb: Bool => value == wb.value
    case b: Boolean => value == b
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class WChar(val value: Char) extends W {
  override def toString = "~" + value
  override def hostChar = value
  override def equals(o: Any) = o match {
    case wc: WChar => value == wc.value
    case c: Char => value == c
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class Dict(val value: HashMap[W, W]) extends W {
  override def toString =
    "{" + value.toList.map(x => (x._1.toString + " " + x._2.toString)).mkString(", ") + "}"
  override def hostHashMap = value
  override def equals(o: Any) = o match {
    case d: Dict => value == d.value
    case i: HashMap[_, _] => value == i
    case _ => false
  }
  override def hashCode = value.hashCode()
}

class WList(val value: Stream[W]) extends W {
  override def hostStream = value
  override def equals(o: Any) = o match {
    case l: WList => value == l.value
    case s: Seq[_] => value == s
    case _ => false
  }

  def evaledArgs(e: HashMap[W, W]) = value.tail.map(Interpretter.eval(e, _))

  override def toString = asString.map('\'' + _ + '\'')
    .getOrElse("(" + value.map(_.toString).mkString(" ") + ")")

  override def hashCode = 0

  override def hostString = asString.get

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

class Num(val value: Int) extends W {
  override def toString = value.toString()
  override def hostInt = value
  override def equals(o: Any) = o match {
    case n: Num => value == n.value
    case i: Int => value == i
    case _ => false
  }
  override def hashCode = value.hashCode()
}

// It's probably more efficient to implement this as a trait, rather than
// boxing a symbol. TODO: investiage
class Sym(val value: Symbol) extends W {
  override def toString = value.name
  override def hostSym = value
  override def equals(o: Any) =
    o match {
      case as: Sym => value == as.value
      case s: Symbol => value == s
      case _ => false
    }
  override def hashCode = value.hashCode()
}

// Pretty much what you'd expect: Takes three arguments: cond trueCase falseCase
// only evalutes trueCase if it needs to, only evalutes falseCase if it needs to
trait If extends W {
  override def toString = "If"
  // implementation in Interpretter, for tail-calls
}

// Takes two arguments, the form and the environment. Then evaluates the form using
// the environment. Remember this function is strict, so the arguments are "double"
// evaluated
trait Eval extends W {
  override def toString = "Eval"
  // implementation in Interpretter, for tail-calls
}

trait ReadFile extends W {
  override def toString = "ReadFile"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(file) = fn.evaledArgs(env)

    new WList(
      scala.io.Source.fromFile(file.hostString).toStream
        .map(new WChar(_) with DerivedFrom { def from = fn })) with DerivedFrom { def from = fn }
  }
}

trait Parse extends W {
  override def toString = "Parse"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(cl) = fn.evaledArgs(env)
    val charList = cl.asInstanceOf[WList]
    Parser(charList.hostString)
  }
}

trait Vau extends W {
  override def toString = "Vau"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val Stream(aS, eS, code) = fn.evaledArgs(env)

    // not entirely sure special casing _ is overly nice, but it seems to work well in practice
    require(aS.isInstanceOf[Sym], "Vau expects that the arg is a symbol, got: " + aS)

    val argSym = aS.asInstanceOf[Sym]

    require(argSym.value == Symbol("_") || !env.contains(argSym), "The environment already contains arg symbol: " + argSym)

    require(eS.isInstanceOf[Sym], "Vau expects that the env is a symbol, got: " + eS)

    val envSym = eS.asInstanceOf[Sym]

    require(envSym.value == Symbol("_") || !env.contains(envSym), "The environment already contains env symbol: " + envSym)

    require(envSym.value == Symbol("_") || argSym != envSym, "ArgSymbol and EnvSymbol: " + argSym + " must not be the same")

    new VauRun(env, argSym, envSym, code)
  }
}

case class VauRun(capEnv: HashMap[W, W], arg: Sym, env: Sym, capCode: W) extends W {
  override def toString = "$UDF$"
  // implementation in Interpretter, for tail-calls
}

object WTypes extends Enumeration {
  type WType = Value
  val TypeBool, TypeSym, TypeNum, TypeDict, TypeFunc, TypeList, TypeType = Value
}

class WType(val value: WTypes.WType) extends W {
  override def toString = "{Type: " + value.toString + "}"
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

// The only non-strict builtin in wisp. Return the first argument unevaluated
trait Quote extends W {
  override def toString = "Quote"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    require(fn.value.length == 2, "Argument to quote must have exactly 1 argument")
    fn.value(1)
  }
  override def equals(o: Any) = o.isInstanceOf[Quote]
}

trait Sequence extends W {
  override def toString = "Sequence"
  // impl in interpretter for tail-calls
}

trait TypeEq extends W {
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

trait ListMake extends W {
  override def toString = "ListMake"
  override def execute(fn: WList, env: HashMap[W, W]) =
    new WList(fn.evaledArgs(env)) with DerivedFrom { def from = fn }
  override def equals(o: Any) = o.isInstanceOf[ListMake]
}

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

trait DictMake extends W {
  override def toString = "DictMake"
  override def execute(fn: WList, env: HashMap[W, W]) = {
    val pairs = fn.evaledArgs(env).map(x => {
      require(x.isInstanceOf[WList])
      val l = x.asInstanceOf[WList].value
      require(l.length == 2)
      (l(0), l(1))
    })

    new Dict(HashMap(pairs.toSeq: _*)) with DerivedFrom { def from = fn }
  }
  override def equals(o: Any) = o.isInstanceOf[DictMake]
}

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
