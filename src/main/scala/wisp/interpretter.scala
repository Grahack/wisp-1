package wisp

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths
  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(new HashMap(), form)

  def eval(e: HashMap[W, W], form: W): W =
    form match {

      case sym: Sym => {
        require(e.contains(sym), "Could not find: " + sym + " in enviornment: " + e)
        e(sym)
      }
      case fnCall: WList =>
        require(fnCall.value.nonEmpty, "Can't evaluate an empty list")
        val fn #:: rawArgs = fnCall.value

        eval(e, fn) match {
          // in order to tail call if/eval, can't just dynamic-dispatch out

          case VauRun(capEnv, argS, envS, capCode) =>
            eval(capEnv +
              (argS -> new WList(rawArgs) with DerivedFrom { def from = fnCall }) +
              (envS -> new Dict(e) with DerivedFrom { def from = fnCall }), capCode)

          case _: If => {
            val Stream(cond, trueCase, falseCase) = rawArgs
            if (eval(e, cond).hostBoolean)
              eval(e, trueCase)
            else
              eval(e, falseCase)
          }

          case _: Eval => {
            val Stream(de, dform) = rawArgs
            eval(eval(e, de).asInstanceOf[Dict].value, eval(e, dform))
          }

          case wf => wf.execute(fnCall, e)
        }
      case x => x
    }
}
