package wisp

import java.nio.file.Path
import java.nio.file.Paths
import scala.PartialFunction

object Interpretter {

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

    DoBlock.run(env, rest)
  }

  private def startingEnv = Dict() +
    ('true -> true) +
    ('false -> false) +
    // Some pretty primitive stuff
    ('do -> DoBlock) +
    ('eval -> Eval) +
    ('if -> If) +
    ('vau -> Vau) +
    (Symbol("'") -> Quote) +
    // some maths stuff
    (Symbol("+") -> Add) +
    (Symbol("-") -> Sub) +
    (Symbol("<") -> LessThan) +
    (Symbol("<=") -> LessThanOrEqual) +
    (Symbol("==") -> Equal) +
    (Symbol(">") -> GreaterThan) +
    (Symbol(">=") -> GreaterThanOrEqual) +
    // utility like
    ('str -> Str) +
    ('assert -> Assert) +
    // vect functions
    ('nth -> Nth) +
    ('length -> Length) +
    ('cons -> Cons) +
    ('list -> VecFunc) + // <-- TODO: RENAME
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

  trait WVal {
    def apply(e: Dict): Any
  }

  trait WProc {
    def strict: Boolean
  }

  def eval(e: Dict, form: Any): Any = {
    form match {
      case s: Symbol => {
        e(s) match {
          case v: WVal => v(e)
          case x => x
        }
      }
      case f +: rawArgs => {

        val which = eval(e, f).asInstanceOf[WProc]

        def evaledArgs() = rawArgs.map(eval(e, _))

        which match {
          case VauRun(capEnv, envS, argS, capCode) => eval(capEnv + (envS -> e) + (argS -> rawArgs), capCode)
          case Eval => evaledArgs() match {
            case Vect(env: Dict, v) => eval(env, v)
          }
          case DoBlock => DoBlock.run(e, rawArgs)._1
          case Equal => foldReduce(_ == _, e, rawArgs)
          case Add => evaledArgs().reduce(_.asInstanceOf[Int] + _.asInstanceOf[Int])
          case Sub => evaledArgs() match {
            case Vect() => 0
            case Vect(v: Int) => -v
            case (head: Int) +: rest => head - rest.reduce(_.asInstanceOf[Int] + _.asInstanceOf[Int]).asInstanceOf[Int]
          }
          case If => rawArgs match {
            case Vect(cond, trueCase, falseCase) => if (eval(e, cond).asInstanceOf[Boolean]) eval(e, trueCase) else eval(e, falseCase)
          }
          case Quote => rawArgs match {
            case Vect(x) => x
          }
          case Vau => rawArgs match {
            case Vect(envS: Symbol, argS: Symbol, code) =>
              require(!e.contains(envS))
              require(!e.contains(argS))
              require(envS != argS)
              VauRun(e, envS, argS, code)
          }
          case LessThan => foldReduce(_ < _, e, rawArgs)
          case LessThanOrEqual => foldReduce(_ <= _, e, rawArgs)
          case GreaterThan => foldReduce(_ > _, e, rawArgs)
          case Str => {
            val sb = new StringBuilder()
            rawArgs.foreach(x => sb.append(eval(e, x)))
            sb.result()
          }
          case Nth => evaledArgs() match {
            case Vect(value: Vect, index: Int) => value(index)
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
              val func = eval(e, f).asInstanceOf[WProc]
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

        }
      }
      case x => x
    }
  }

  object Eval extends WProc {
    def strict = true
  }

  object Add extends WProc {
    def strict = true
  }

  object Sub extends WProc {
    def strict = true
  }

  object If extends WProc {
    def strict = false
  }

  object Quote extends WProc {
    def strict = false
  }

  object Vau extends WProc {
    def strict = false
  }

  case class VauRun(capEnv: Dict, envS: Symbol, argS: Symbol, capCode: Any) extends WProc {
    def strict = false
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

  object LessThan extends WProc {
    def strict = false
  }

  object LessThanOrEqual extends WProc {
    def strict = false
  }

  object Equal extends WProc {
    def strict = false
  }

  object GreaterThan extends WProc {
    def strict = false
  }

  object GreaterThanOrEqual extends WProc {
    def strict = false
  }

  object DoBlock extends WProc {

    def strict = false

    def run(e: Dict, forms: Vect): (Any, Dict) = {
      val (lets, rest) = forms.partition {
        _ match {
          case 'let +: (s: Symbol) +: v +: Vect() => true
          case 'let +: _ => sys.error("Malformed let")
          case _ => false
        }
      }

      object LetResult { def apply(v: Any) = new LetResult(v, false) }

      class LetResult(var payload: Any, var hasBeenEval: Boolean) extends WVal {

        def apply(u: Dict): Any = {

          if (!hasBeenEval) {
            val (capEnv, original) = payload.asInstanceOf[(Dict, Any)]
            payload = eval(capEnv, original)
            hasBeenEval = true
          }
          payload
        }
        def setEnv(e: Dict) = {
          payload = (e -> payload)
        }
      }

      val allLets = lets.data.map(x => { val y = x.asInstanceOf[Vect]; (y(1).asInstanceOf[Symbol] -> LetResult(y(2))) })

      // Now, let's add them all to a new environment

      val newEnv = allLets.foldLeft(e) {
        (oldEnv, form) =>
          require(!oldEnv.contains(form._1), "Can't redefine a symbol: " + form._1)
          oldEnv + form
      }

      // Now all our let's need a reference to the env they were defined in

      allLets.foreach { l => l._2.setEnv(newEnv) }

      // now that we done all our static environment stuff, we can go through and evaluate it all

      (rest.foldLeft(Vect(): Any)((a, b) => eval(newEnv, b)), newEnv)
    }

  }

  object Str extends WProc {
    def strict = true
  }

  object Nth extends WProc {
    def strict = true
  }

  object Cons extends WProc {
    def strict = true
  }

  object VecFunc extends WProc {
    def strict = true
  }

  object Trace extends WProc {
    def strict = true
  }

  object Fails extends WProc {
    def strict = false
  }

  object Assert extends WProc {
    def strict = true
  }

  object Length extends WProc {
    def strict = true
  }

  object DictSize extends WProc {
    def strict = true
  }

  object DictGet extends WProc {
    def strict = true
  }

  object FoldLeft extends WProc {
    def strict = false
  }

  object DictInsert extends WProc {
    def strict = true
  }

  object DictRemove extends WProc {
    def strict = true
  }

  object DictContains extends WProc {
    def strict = true
  }

  object Not extends WProc {
    def strict = true
  }

  object And extends WProc {
    def strict = false
  }

  object Or extends WProc {
    def strict = false
  }

}
