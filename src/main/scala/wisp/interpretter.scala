package wisp

import com.sun.org.apache.xpath.internal.objects.GreaterThanOrEqualComparator
import scala.xml.TypeSymbol

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths

  def apply(path: Path): (Any, Dict) = {

    val forms = Reader(path)

    val (imports, rest) = forms.span(_ match {
      case 'import +: _ => true
      case _ => false
    })

    val env = if (imports.isEmpty) startingEnv else {

      imports.foldLeft(Dict())((e, nextImport) => nextImport match {
        case 'import +: (importFilePath: String) +: Vect() => {
          // TODO: avoid double-loading a file
          e merge Interpretter(path.resolveSibling(importFilePath))._2
        }
        case _ => sys.error("Could not understand import")
      })

    }

    val (statements, newEnv) = buildDoBlock(env, rest)

    (statements.foldLeft(Vect(): Any)((a, b) => eval(newEnv, b)) -> newEnv)
  }

  object WFunc extends Enumeration {
    type WFunc = Value
    val TypeOf, TypeEq, Str, Nth, Drop, Cons, VecFunc, Trace, Fails, Assert, Length, DictSize, DictGet, FoldLeft, DictInsert, DictRemove, DictContains, Not, And, Or, LessThan, LessThanOrEqual, NumEq, GreaterThan, GreaterThanOrEqual, Eval, Add, Sub, If, Quote, Vau, DoBlock, NumToString = Value
  }

  object WTypes extends Enumeration {
    type WType = Value
    val TypeBool, TypeSym, TypeNum, TypeDict, TypeStr, TypeVect, TypeType = Value
  }

  import WTypes._
  import WFunc._

  private def startingEnv = Dict() +
    ('true -> true) +
    ('false -> false) +
    // Types
    ('Num -> TypeNum) +
    ('Str -> TypeStr) +
    ('Bool -> TypeBool) +
    ('Vect -> TypeVect) +
    ('Sym -> TypeSym) +
    ('Dict -> TypeDict) +
    ('Type -> TypeType) +
    (Symbol("type-of") -> TypeOf) +
    (Symbol("type-eq") -> TypeEq) +
    // Some pretty primitive stuff
    ('do -> DoBlock) +
    ('eval -> Eval) +
    ('if -> If) +
    ('vau -> Vau) +
    (Symbol("'") -> Quote) +
    // some num stuff
    (Symbol("num-add") -> Add) +
    (Symbol("num-sub") -> Sub) +
    (Symbol("num-lt") -> LessThan) +
    (Symbol("num-lte") -> LessThanOrEqual) +
    (Symbol("num-eq") -> NumEq) +
    (Symbol("num-gt") -> GreaterThan) +
    (Symbol("num-gte") -> GreaterThanOrEqual) +
    (Symbol("num-to-str") -> NumToString) +
    // utility like
    ('str -> Str) +
    ('assert -> Assert) +
    // vect functions
    ('nth -> Nth) +
    ('drop -> Drop) +
    ('length -> Length) +
    ('cons -> Cons) +
    ('vect -> VecFunc) +
    (Symbol("fold-left") -> FoldLeft) +
    // Dict functions
    (Symbol("dict-insert") -> DictInsert) +
    (Symbol("dict-size") -> DictSize) +
    (Symbol("dict-contains") -> DictContains) +
    (Symbol("dict-get") -> DictGet) +
    (Symbol("dict-remove") -> DictRemove) +
    // boolean
    ('not -> Not) +
    ('and -> And) +
    ('or -> Or) +
    // debug
    ('trace -> Trace) +
    ('fails -> Fails)

  def eval(e: Dict, form: Any): Any = {
    form match {
      case lr: LetResult => lr()
      case s: Symbol => {
        require(s != Symbol("_"), "You really shouldn't use _ as a resolvable symbol")
        e(s) match {
          case lr: LetResult => lr()
          case x => x
        }
      }

      case f +: rawArgs => {

        def evaledArgs() = rawArgs.map(eval(e, _))

        eval(e, f) match {
          case VauRun(capEnv, envS, argS, capCode) => eval(capEnv + (envS -> e) + (argS -> rawArgs), capCode)
          case Eval => evaledArgs() match {
            case Vect(env: Dict, v) => eval(env, v)
          }
          case DoBlock => {
            val (statements, newEnv) = buildDoBlock(e, rawArgs)
            require(statements.nonEmpty)
            statements.init.foreach {
              eval(newEnv, _)
            }
            // and for a nice tail call
            eval(newEnv, statements.last)
          }
          case NumEq => evaledArgs() match {
            case Vect(a: Int, b: Int) => a == b
          }
          case Add => evaledArgs() match {
            case Vect(a: Int, b: Int) => a + b
          }
          case Sub => evaledArgs() match {
            case Vect(a: Int, b: Int) => a - b
          }
          case If => rawArgs match {
            case Vect(cond, trueCase, falseCase) => if (eval(e, cond).asInstanceOf[Boolean]) eval(e, trueCase) else eval(e, falseCase)
          }
          case Quote => rawArgs match {
            case Vect(x) => x
          }
          case Vau => rawArgs match {
            case Vect(envS: Symbol, argS: Symbol, code) =>
              require(!e.contains(envS), "Can't use symbol " + envS + " for binding an environment, as it already exists")
              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
              require(envS != argS)
              VauRun(e, envS, argS, code)
          }
          case LessThan => evaledArgs() match {
            case Vect(a: Int, b: Int) => a < b
          }
          case LessThanOrEqual => evaledArgs() match {
            case Vect(a: Int, b: Int) => a <= b
          }
          case GreaterThan => evaledArgs() match {
            case Vect(a: Int, b: Int) => a > b
          }
          case GreaterThanOrEqual => evaledArgs() match {
            case Vect(a: Int, b: Int) => a >= b
          }
          case NumToString => evaledArgs() match {
            case Vect(a: Int) => a.toString()
          }
          case TypeOf => evaledArgs() match {
            case Vect(a) => a match {
              case _: Boolean => TypeBool
              case _: Int => TypeNum
              case _: String => TypeStr
              case _: Symbol => TypeSym
              case _: Vect => TypeVect
              case _: Dict => TypeDict
              case _: WType => TypeType
            }
          }
          case Str => {
            val sb = new StringBuilder()
            rawArgs.foreach(x => sb.append(eval(e, x)))
            sb.result()
          }
          case Nth => evaledArgs() match {
            case Vect(value: Vect, index: Int) => value(index)
          }
          case Drop => evaledArgs() match {
            case Vect(value: Vect, amount: Int) => value.drop(amount)
          }
          case Cons => evaledArgs() match {
            case Vect(h, tail: Vect) => tail.cons(h)
          }
          case VecFunc => evaledArgs()
          case Trace => println(evaledArgs().mkString)
          case Fails => rawArgs match {
            case Vect(arg) => try {
              eval(e, arg)
              false
            } catch {
              case _ => true
            }
          }
          case Assert => evaledArgs() match {
            case Vect(res: Boolean) => require(res, "Code assertion failed!")
            case Vect(res: Boolean, msg: String) => require(res, "Code assertion failed, with errror: " + msg)
          }
          case Length => evaledArgs() match {
            case Vect(vec: Vect) => vec.length
          }
          case DictSize => evaledArgs() match {
            case Vect(dict: Dict) => dict.size
          }
          case DictGet => evaledArgs() match {
            case Vect(dict: Dict, k) => dict(k)
          }
          case FoldLeft => rawArgs match {
            case Vect(v, start, f) =>
              val vec = eval(e, v).asInstanceOf[Vect]
              // Note: note evaling 'start' as we're threading it through
              val func = eval(e, f)
              eval(e, vec.foldLeft(start)((a, b) => Vect(Quote, eval(e, Vect(func, a, b)))))
          }
          case DictInsert => evaledArgs() match {
            case Vect(dict: Dict, k, v) => dict + (k -> v)
          }
          case DictRemove => evaledArgs() match {
            case Vect(dict: Dict, k) => dict - k
          }
          case DictContains => evaledArgs() match {
            case Vect(lu: Dict, k) => lu.contains(k)
          }
          case Not => evaledArgs() match {
            case Vect(arg: Boolean) => !arg
          }
          case And => {
            require(rawArgs.length >= 2)
            rawArgs.data.forall(eval(e, _).asInstanceOf[Boolean] == true)
          }
          case Or => {
            require(rawArgs.length >= 2)
            !rawArgs.data.forall(eval(e, _).asInstanceOf[Boolean] == false)
          }
          case TypeEq => evaledArgs() match {
            case Vect(a: WType, b: WType) => a == b
          }

        }
      }
      case x => x
    }
  }

  case class VauRun(capEnv: Dict, envS: Symbol, argS: Symbol, capCode: Any) {
    override def toString = "$vau$" + this
  }

  def foldReduce(op: (Int, Int) => Boolean, e: Dict, args: Vect): Boolean = {
    require(args.length > 1)

    var acc = eval(e, args.head).asInstanceOf[Int]

    args.tail.foreach { a =>
      val r = eval(e, a).asInstanceOf[Int]
      if (op(acc, r))
        acc = r
      else
        return false
    }

    true
  }

  def strict = false

  def buildDoBlock(e: Dict, forms: Vect): (IndexedSeq[Any], Dict) = {
    def letBuilder(form: Any, v: LetResult): Iterable[(Option[Symbol], LetResult)] = {

      form match {
        case Symbol("_") => None
        case s: Symbol => Iterable(Some(s) -> v)
        case pattern: Vect => {
          require(pattern.nonEmpty)

          val (single, multi) = pattern.span(_ != Symbol("&"))

          val built = (None -> v) +: single.data.zipWithIndex.flatMap {
            case (s, i) => letBuilder(s, LetResult(Vect(Nth, v, i)))
          }

          if (multi.nonEmpty) {
            assert(multi(0) == Symbol("&"))
            require(multi.length == 2)

            v.setCheck(_.asInstanceOf[Vect].length >= single.length)

            (Some(multi(1).asInstanceOf[Symbol]) -> LetResult(Vect(Drop, v, single.length))) +: built

          } else {
            v.setCheck(_.asInstanceOf[Vect].length == single.length)
            built
          }

        }
        case x => sys.error("Unknown form: " + x + " in let")
      }

    }

    val lets = forms.data.flatMap {
      _ match {
        case Vect('let, binding, v) => letBuilder(binding, LetResult(v))
        case 'let +: _ => sys.error("Malformed let statement")
        case _ => None
      }
    }

    val rest = forms.data.flatMap {
      _ match {
        case 'let +: _ => None
        case x => Some(x)
      }
    }

    // Now, let's add them all to a new environment

    val newEnv = lets.foldLeft(e) {
      (oldEnv, form) =>
        form._1.map {
          name =>
            require(!oldEnv.contains(name), "Can't redefine a symbol: " + name)
            oldEnv + (name -> form._2)
        }.getOrElse(oldEnv)
    }

    // Now all our let's need a reference to the env they were defined in

    lets.foreach { l => l._2.setEnv(newEnv) }

    // now that we done all our static environment stuff, we can go through and evaluate it all

    (rest, newEnv)
  }

  object LetResult { def apply(v: Any) = new LetResult(v, false) }

  class LetResult(var payload: Any, var hasBeenEval: Boolean, var check: Any => Boolean = _ => true) {

    def apply(): Any = {

      if (!hasBeenEval) {
        val (capEnv, original) = payload.asInstanceOf[(Dict, Any)]
        payload = eval(capEnv, original)

        require(check(payload))

        hasBeenEval = true
      }
      payload
    }

    def setCheck(c: Any => Boolean) = {
      check = c
    }

    def setEnv(e: Dict) = {
      payload = (e -> payload)
    }
  }

}
