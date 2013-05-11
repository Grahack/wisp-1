package espringe.wisp

object Interpretter {

  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(new HashMap(), form)

  def eval(e: Dict, form: W): W = {

    object BoolEval {
      def unapply(value: W) = value.asBool
    }
    object DictEval {
      def unapply(value: W) = value.asDict
    }

    form match {

      case sym: Sym => {
        require(e.contains(sym), "Could not find: " + sym + " in enviornment: " + e)
        e(sym)
      }
      case fnCall: WList =>
        require(fnCall.value.nonEmpty, "Can't evaluate an empty list")
        val fn #:: rawArgs = fnCall.value // TODO: properly...

        eval(e, fn) match {
          // in order to tail call if/eval, can't just dynamic-dispatch out

          case UDF(capEnv, argS, envS, capCode) =>
            require(rawArgs.isEmpty)
            eval(capEnv + (argS -> WList(rawArgs)) + (envS -> WDict(e)), capCode)
          case If() =>
            val Stream(BoolEval(cond), trueCase, falseCase) = rawArgs
            eval(e, if (cond) trueCase else falseCase)
          case Eval() =>
            val Stream(DictEval(ue), uform) = rawArgs
            eval(ue, eval(e, uform))

          case DictMake() => ???
          case ListMake() => ???
          case Parse() => ???
          case Quote() => ???
          case ReadFile() => ???
          case Vau() => ???
          
          
          
          case WChar(x) => sys.error(s"Cannot evaluate a Char. $x in $fnCall")
          case WDict(x) => sys.error(s"Cannot evalute a Dict. $x in $fnCall")
          case WList(x) => sys.error(s"Cannot evalute a List? $x in $fnCall")
          case Sym(x) => sys.error(s"Cannot evaluate a Symbol? $x in $fnCall")
          case WType(x) => sys.error(s"Cannot evalute a Type. $x in $fnCall")
          case Bool(x) => sys.error(s"Cannot evalute a Boolean. $x in $fnCall")
          case Num(x) => sys.error(s"Cannot evaluate a Num. $x in $fnCall")

        }
      case x => x
    }
  }
}
