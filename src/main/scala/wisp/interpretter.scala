package wisp

object Interpretter {
  def eval(in: Any, env: Environment): Any = in match {
    case l: List[_] => env.get(l.head.asInstanceOf[Symbol]).asInstanceOf[(Any, Environment) => Any](l, env)
    case s: Symbol =>  env.get(s)
    case x => x
  }
}