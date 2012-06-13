package wisp

object Interpretter {

  type Procedure = (List[Any], Environment) => (Any, Environment)

  // Special forms

  def vau(args: List[Any], env: Environment): (Any, Environment) =
    args match {
      case a :: e :: body :: Nil => {
        val argSymbol = a.asInstanceOf[Symbol]
        val envSymbol = a.asInstanceOf[Symbol]

        ((callerArgs: Any, callerEnv: Environment) => {
          var newEnv = callerEnv ++ env + (argSymbol, callerArgs) + (envSymbol, callerEnv)

          (resolve(body, newEnv), callerEnv)
        }, env)
      }
      case _ => sys.error("vau expected 3 arguments, got: " + args + " instead")
    }

  def eval(args: List[Any], env: Environment): (Any, Environment) = {
    resolve(args.head, env)
  }

  def ifProcedure(args: List[Any], env: Environment): (Any, Environment) =
    args match {
      case cond :: trueCase :: falseCase :: Nil => {
        if (resolve(cond, env)._1.asInstanceOf[Boolean])
          resolve(trueCase, env)
        else
          resolve(falseCase, env)
      }
      case _ => sys.error("if statement was expecting 3 args, found: " + args.length)
    }

  def define(args: List[Any], env: Environment): (Any, Environment) =
    args match {
      case symbol :: value :: Nil => ((), env + (symbol.asInstanceOf[Symbol], value))
      case _ => sys.error("define expected 2 arguments, got: " + args + " instead")
    }

  // helper functions

  def resolve(in: Any, env: Environment): (Any, Environment) = in match {
    case l: List[_] => env(l.head.asInstanceOf[Symbol]).asInstanceOf[Procedure](l.tail, env)
    case s: Symbol => (env(s), env)
    case x => (x, env)
  }

  def strict(f: List[Any] => Any)(args: List[Any], env: Environment) =
    (f(args.map(x => resolve(x, env))), env)

  // builtin functions

  def addition = strict((args: List[Any]) => args.foldLeft(0)((a, b) => a + b.asInstanceOf[Int])) _
  def print = strict((args: List[Any]) => println(args)) _
  def subtract = strict((args: List[Any]) => args.head.asInstanceOf[Int] - args.tail.foldLeft(0)((a, b) => a + b.asInstanceOf[Int])) _
  def head = strict((args: List[Any]) => args.map(_.asInstanceOf[List[_]].head)) _
  def tail = strict((args: List[Any]) => args.map(_.asInstanceOf[List[_]].tail)) _

}
