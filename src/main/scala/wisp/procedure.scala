package wisp

trait Procedure {
  def apply(args: List[Any], env: Environment): Any
}

trait StrictProcedure extends Procedure {
  def apply(args: List[Any], env: Environment): Any = {
    run(args.map(x => Eval.onAtom(x, env)))
  }

  def run(args: List[Any]): Any
}

object Subtract extends StrictProcedure {
  def run(args: List[Any]) = {
    val head = args.head.asInstanceOf[Int]
    val rest = Plus.run(args.tail).asInstanceOf[Int]

    head - rest
  }
}

object Define extends Procedure {
  def apply(args: List[Any], env: Environment): Any =
    args match {
      case symbol :: value :: Nil => env.add(symbol.asInstanceOf[Symbol], value)
      case _ => sys.error("define expected 2 arguments, got: " + args.length + " instead")
    }
}

object Plus extends StrictProcedure {
  def run(args: List[Any]) =
    args.foldLeft(0)((a, b) => a + b.asInstanceOf[Int])
}

object Eval extends Procedure {
  def apply(args: List[Any], env: Environment): Any = {
    if (args.length != 1)
      sys.error("Was expecting a single argument for eval, but got " + args.length + " instead")

    onAtom(args.head, env)
  }

  def onAtom(in: Any, env: Environment): Any = in match {
    case l: List[_] => env.get(l.head.asInstanceOf[Symbol]).asInstanceOf[(Any, Environment) => Any](l, env)
    case s: Symbol => env.get(s).getOrElse(sys.error("Was searching for symbol '" + s + "' but couldn't find it."))
    case x => x
  }
}