package wisp

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths
  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(form, new HashMap())

  def eval(form: W, e: HashMap[W, W]): W = form match {
    case sym: Sym => {
      require(e.contains(sym), "Could not find: " + sym + " in enviornment: " + e)
      e(sym)
    }
    case fnCall: WList =>
      require(fnCall.value.nonEmpty, "Can't evaluate an empty list")
      val fn #:: rawArgs = fnCall.value

      eval(fn, e) match {
        // in order to tail call if/eval, can't just dynamic-dispatch out

        case VauRun(capEnv, argS, envS, capCode) =>
          eval(capCode, capEnv +
              (argS -> new WList(rawArgs) with DerivedFrom { def from = fnCall })  + 
              (envS -> new Dict(e) with DerivedFrom { def from = fnCall }))

        case _: If => {
          val Stream(cond, trueCase, falseCase) = rawArgs
          if (eval(cond, e).hostBool)
            eval(trueCase, e)
          else
            eval(falseCase, e)
        }
        
        case _: Eval => {
          val Stream(dform, de) = rawArgs
          eval(eval(dform,e), eval(de, e).asInstanceOf[Dict].value)
        }

        case wf => wf.execute(fnCall, e)
      }
    case x => x
  }

}
