package wisp

trait Procedure {
  def apply(args: List[Any], env: Environment): Any
}

object Vau extends Procedure {
  def apply(args: List[Any], env: Environment): Any =
    args match {
      case a :: e :: body :: Nil => {
        val argSymbol = a.asInstanceOf[Symbol]
        val envSymbol = a.asInstanceOf[Symbol]
        
        new Procedure {
          def apply(callerArgs: List[Any], callerEnv: Environment) = {
            val newEnv = env.copyOnto(Some(callerEnv))
            callerEnv.set(argSymbol, callerArgs)
            callerEnv.set(envSymbol, callerEnv)
            Eval.onAtom(body, callerEnv)
          }
        }
      }
      case _ => sys.error("vau expected 3 arguments, got: " + args.length + " instead")
    }
}

object Eval extends Procedure {
  def apply(args: List[Any], env: Environment) =
    env.get(args.head.asInstanceOf[Symbol]).asInstanceOf[Procedure](args.tail, env)

  def onAtom(in: Any, env: Environment): Any = in match {
    case l: List[_] => Eval(l, env)
    case s: Symbol => env.get(s)
    case x => x
  }
}

object If extends Procedure {
  def apply(args: List[Any], env: Environment) =
    args match {
      case cond :: trueCase :: falseCase :: Nil => {
        if (Eval.onAtom(cond, env).asInstanceOf[Boolean])
          Eval.onAtom(trueCase, env)
        else
          Eval.onAtom(falseCase, env)
      }
      case _ => sys.error("if statement was expecting 3 args, found: " + args.length)
    }
}

object Define extends Procedure {
  def apply(args: List[Any], env: Environment): Any =
    args match {
      case symbol :: value :: Nil => env.set(symbol.asInstanceOf[Symbol], value)
      case _ => sys.error("define expected 2 arguments, got: " + args.length + " instead")
    }
}

trait StrictProcedure extends Procedure {
  def apply(args: List[Any], env: Environment): Any = {
    run(args.map(x => Eval.onAtom(x, env)))
  }

  def run(args: List[Any]): Any
}

object Id extends StrictProcedure { // TODO: implement in core.wisp ?
  def run(args: List[Any]): Any = if (args.length == 1) args.head else args
}

object Head extends StrictProcedure {
  def run(args: List[Any]) =
    args.head
}

object Tail extends StrictProcedure {
  def run(args: List[Any]) =
    args.tail
}

object Print extends StrictProcedure {
  def run(args: List[Any]) =
    args foreach print
}

object Subtract extends StrictProcedure {
  def run(args: List[Any]) = {
    val head = args.head.asInstanceOf[Int]
    val rest = Addition.run(args.tail)

    head - rest
  }
}

object Addition extends StrictProcedure {
  def run(args: List[Any]) =
    args.foldLeft(0)((a, b) => a + b.asInstanceOf[Int])
}