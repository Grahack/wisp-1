package wisp

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths
  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(form, new HashMap())

  def eval(form: W, e: HashMap[W, W]): W = form match {
    case sym: WSym => {
      require(e.contains(sym), "Could not find: " + sym + " in enviornment")
      e(sym)
    }
    case fnCall: WList =>
      require(fnCall.value.nonEmpty, "Can't evaluate an empty list")
      val fn #:: rawArgs = fnCall.value

      eval(fn, e) match {
        // in order to tail call these, can't just dynamic-dispatch out

        case WLambdaRun(capEnv, argS, capCode) =>
          eval(capCode, capEnv + (argS -> new WParamList("{rawFunc: " + fn.summary + "}", e, rawArgs)))

        case _: WIf => {
          val Stream(cond, trueCase, falseCase) = rawArgs
          if (eval(cond, e).hostBool)
            eval(trueCase, e)
          else
            eval(falseCase, e)
        }

        case wf => rawArgs match {
          case Stream(a) => wf.execute1(fnCall, eval(a, e))
          case Stream(a, b) => wf.execute2(fnCall, eval(a, e), eval(b, e))
          case Stream(a, b, c) => wf.execute3(fnCall, eval(a, e), eval(b, e), eval(c, e))
          case _ => wf.executeN(fnCall, rawArgs.map(eval(_, e)))
        }
      }
    case x => x
  }

}
