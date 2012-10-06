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
      case f #:: rawArgs => {
        eval(f, e) match {
          case VauRun(capEnv, argS, envS, capCode) => eval(capCode, capEnv + (argS -> rawArgs) + (envS -> e))
          case Vau => rawArgs match {
            case Stream(argS: Symbol, envS: Symbol, code) =>
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
            case WList(Eval, v, env: Dict) => eval(v, env)

            // type stuff
            case WList(TypeEq, a: WType, b: WType) => a == b
            case WList(TypeOf, a) => a match {
              case _: Boolean => TypeBool
              case _: Dict => TypeDict
              case _: WFunc | VauRun | Vau | If => TypeFunc
              case _: Int => TypeNum
              case _: Symbol => TypeSym
              case _: WType => TypeType
              case _: IsWList => TypeList
            }
            case WList(NumAdd, a: Int, b: Int) => a + b
            case WList(NumDiv, a: Int, b: Int) => a / b
            case WList(NumEq, a: Int, b: Int) => a == b
            case WList(NumNeq, a: Int, b: Int) => a != b
            case WList(NumGreaterThan, a: Int, b: Int) => a > b
            case WList(NumGreaterThanOrEqual, a: Int, b: Int) => a >= b
            case WList(NumLessThan, a: Int, b: Int) => a < b
            case WList(NumLessThanOrEqual, a: Int, b: Int) => a <= b
            case WList(NumMult, a: Int, b: Int) => a * b
            case WList(NumSub, a: Int, b: Int) => a - b
            case WList(SymEq, a: Symbol, b: Symbol) => a == b
            case WList(ListCons, list: IsWList, v) => v +: list
            case WList(DictContains, dict: Dict, k) => dict.contains(k)
            case WList(DictGet, dict: Dict, k) => dict(k)
            case WList(DictInsert, dict: Dict, k, v) => dict + (k -> v)
            case WList(DictRemove, dict: Dict, k) => dict - k
            case WList(DictSize, dict: Dict) => dict.size
            case WList(DictToList, a: Dict) => a.data.toStream
            case WList(BoolNot, arg: Boolean) => !arg
            case WList(BoolEq, a: Boolean, b: Boolean) => a == b
            case WList(Error) => sys.error("Code called an error")
            case WList(Error, msg) => sys.error("Code called an errror with msg: " + msg)
            case Trace #:: args => println(args)
            case WList(ListLength, vec: IsWList) => vec.length
            case x => sys.error("Unexpected arguments with: " + x)
          }

          case x => sys.error("When evaluating a WList, the first argument was an unexpected: " + x)
        }
      }
      case x => x
    }
  }

  case class VauRun(capEnv: Dict, argS: Symbol, envS: Symbol, capCode: Any) {
    override def toString = "$vau$"
  }

}
