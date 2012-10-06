package wisp

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths

  def apply(form: Any, e: Dict): Any = eval(form, e)

  import WTypes._
  import WFunc._

  def eval(form: Any, e: Dict): Any = {

    form match {
      case s: Symbol => eval(e(s), e)
      case f +: rawArgs => {
        eval(f, e) match {
          case VauRun(capEnv, argS, envS, capCode) => eval(capCode, capEnv + (argS -> rawArgs) + (envS -> e))
          case Vau => rawArgs match {
            case Vect(argS: Symbol, envS: Symbol, code) =>
              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
              require(!e.contains(envS), "Can't use  symbol " + envS + " for binding an environment, as it already exists")
              require(envS != argS, "Can't use the same symbol for binding the environment and argument")
              VauRun(e, argS, envS, code)
            case x => sys.error("#vau expects three arguments, an arg symbol, an env symbol, and the body of the code. Instead found: " + rawArgs)
          }
          case If => {
            require(rawArgs.length == 3, "If statement require three arguments (cond, trueCase, false), was given: " + rawArgs)
            val cond = eval(rawArgs(0), e)
            require(cond.isInstanceOf[Boolean], "Condition in #if statement, should evalute to a boolean -- but instead got: " + cond + ". If statement was: " + rawArgs)
            if (cond.asInstanceOf[Boolean]) eval(rawArgs(1), e) else eval(rawArgs(2), e)
          }
          case wf: WFunc => (wf +: rawArgs.map(eval(_, e))) match {
            case Vect(Eval, v, env: Dict) => eval(v, env)

            // type stuff
            case Vect(TypeEq, a: WType, b: WType) => a == b
            case Vect(TypeOf, a) => a match {
              case _: Boolean => TypeBool
              case _: Dict => TypeDict
              case _: WFunc | VauRun | Vau | If => TypeFunc
              case _: Int => TypeNum
              case _: Symbol => TypeSym
              case _: WType => TypeType
              case _: Vect => TypeVect
            }
            case Vect(NumAdd, a: Int, b: Int) => a + b
            case Vect(NumDiv, a: Int, b: Int) => a / b
            case Vect(NumEq, a: Int, b: Int) => a == b
            case Vect(NumNeq, a: Int, b: Int) => a != b
            case Vect(NumGreaterThan, a: Int, b: Int) => a > b
            case Vect(NumGreaterThanOrEqual, a: Int, b: Int) => a >= b
            case Vect(NumLessThan, a: Int, b: Int) => a < b
            case Vect(NumLessThanOrEqual, a: Int, b: Int) => a <= b
            case Vect(NumMult, a: Int, b: Int) => a * b
            case Vect(NumSub, a: Int, b: Int) => a - b
            case Vect(NumToVect, a: Int) => Vect.fromSeq(a.toString().toSeq)
            case Vect(SymEq, a: Symbol, b: Symbol) => a == b
            case Vect(VectAppend, vect: Vect, v) => vect :+ v
            case Vect(VectCons, vect: Vect, v) => v +: vect
            case Vect(VectNth, vect: Vect, index: Int) => vect(index)
            case Vect(DictContains, dict: Dict, k) => dict.contains(k)
            case Vect(DictGet, dict: Dict, k) => dict(k)
            case Vect(DictInsert, dict: Dict, k, v) => dict + (k -> v)
            case Vect(DictRemove, dict: Dict, k) => dict - k
            case Vect(DictSize, dict: Dict) => dict.size
            case Vect(DictToVect, a: Dict) => Vect.fromSeq(a.data.toSeq) //foldLeft(Vect()) { (p, n) => p.append(n) }
            case Vect(BoolNot, arg: Boolean) => !arg
            case Vect(BoolEq, a: Boolean, b: Boolean) => a == b
            case Vect(Error) => sys.error("Code called an error")
            case Vect(Error, msg) => sys.error("Code called an errror with msg: " + msg)
            case Trace +: args => println(args)
            case Vect(VectLength, vec: Vect) => vec.length
            case x => sys.error("Unexpected arguments with: " + x)
          }

          case x => sys.error("When evaluating a vect, the first argument was an unexpected: " + x)
        }
      }
      case x => x
    }
  }

  case class VauRun(capEnv: Dict, argS: Symbol, envS: Symbol, capCode: Any) {
    override def toString = "$vau$"
  }

}
