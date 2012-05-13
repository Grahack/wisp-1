package wisp

object Environment {
  def apply(): Environment = {
    val e = new Environment(Map[Symbol, Any](), null)

    e.add(Symbol("#f"), false)
    e.add(Symbol("#t"), true)

    e.add('if, builtinIf _)
    e.add('def, builtinDef _)
    e.add(Symbol("+"), builtinPlus _)
    e.add(Symbol("-"), builtinSubtract _)
    e.add('lambda, builtinLambda _)
    e.add(Symbol(">"), builtinGreaterThan _)

    e
  }

  private def builtinIf(in: List[Any], env: Environment) =
    Interpretter.eval(if (Interpretter.eval(in(1), env).asInstanceOf[Boolean]) in(2) else in(3), env)
    
  private def abortFold(op: (Any, Any) => Boolean, in: List[Any], env: Environment): Any = {
    
    var vals = in.tail
    var last: Any = Interpretter.eval(vals.head, env)
    vals = vals.tail
    
    do {
      val n = Interpretter.eval(vals.head, env)
      
      if (!op(last, n))
        return false
      
      last = n
      vals = vals.tail
      
    } while (!vals.isEmpty)
    
    true
  }
  
  private def builtinGreaterThan(in: List[Any], env: Environment) = abortFold((a: Any, b: Any) => (a.asInstanceOf[Int] > b.asInstanceOf[Int]), in, env)
    

  private def builtinDef(in: List[Any], env: Environment) =
    env.add(in(1).asInstanceOf[Symbol], Interpretter.eval(in(2), env))

  private def builtinPlus(in: List[Any], env: Environment) =
    in.tail.foldLeft(0)((a: Int, b: Any) => a + Interpretter.eval(b, env).asInstanceOf[Int])
    
  private def builtinSubtract(in: List[Any], env: Environment) = {
    
    val vals = in.tail
    
    val first = Interpretter.eval(vals.head, env).asInstanceOf[Int]
    
    vals.tail.foldLeft(first)((a: Int, b: Any) => a - Interpretter.eval(b, env).asInstanceOf[Int])
  }
  private def builtinLambda(in: List[Any], env: Environment) =
    (x: List[Any], y: Environment) => evalLambda(in(1).asInstanceOf[List[Symbol]], env.lookup, in(2), x, y)

  private def evalLambda(vars: List[Symbol], capturedEnv: Map[Symbol, Any], body: Any, in: List[Any], env: Environment) = {
    val e = new Environment(capturedEnv, env)

    var args = in.tail

    vars.foreach(v => {
      e.add(v, Interpretter.eval(args.head, env))
      args = args.tail
    })

    Interpretter.eval(body, e)
  }

}

class Environment(private var lookup: Map[Symbol, Any], private val outter: Environment) {
  def add(s: Symbol, a: Any) {
    lookup = lookup + ((s, a))
  }

  def get(s: Symbol): Any = lookup.get(s).getOrElse {
    if (outter != null) outter.get(s) else throw new RuntimeException("Could not find symbol: " + s)
  }
}