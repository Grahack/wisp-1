package wisp

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths
  import scala.collection.immutable.HashMap

  def apply(form: W, e: HashMap[W, W]): W = eval(form, e)

  def eval(form: W, e: HashMap[W, W]): W = {
    form match {
      case sym: WSym => {
        require(e.contains(sym), "Could not find: " + sym + " in enviornment")
        e(sym)
      }
      case stream: WList =>
        require(stream.value.nonEmpty, "Can't evaluate an empty list")
        val fn #:: rawArgs = stream.value

        eval(fn, e) match {
          case WLambdaRun(capEnv, argS, capCode) =>
            eval(capCode, capEnv + (argS -> new WParamList("{rawFunc: " + fn.summary + "}", e, rawArgs)))

          case WLambda => rawArgs match {
            case Stream(argS: WSym, code: W) =>
              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
              WLambdaRun(e, argS, code)
          }
          case WIf => rawArgs match {
            case Stream(cond, trueCase, falseCase) =>
              if (eval(cond, e).rawBool) eval(trueCase,e) else eval(falseCase,e)
            case x =>
              sys.error("Trying to call if without (if trueCase falseCase): " + rawArgs)
          }
          case wf: WFunc => wf(rawArgs.map(eval(_, e)))
          case x => sys.error("Can't evaluate: " + x.summary + " as a function")
        }
      case x => x
    }
  }

}
