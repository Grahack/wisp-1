package wisp

object Interpretter {

  type Environment = Map[Any, Any]

  type Procedure = (List[Any], Environment) => (Any, Environment)

  // Special forms

  def lambda(args: List[Any], env: Environment): (Any, Environment) =
    args match {
      case a :: e :: body :: Nil => {
        val argSymbol = a.asInstanceOf[Symbol]
        val envSymbol = a.asInstanceOf[Symbol]

        ((callerArgs: Any, callerEnv: Environment) => {
          var newEnv = callerEnv ++ env + ((argSymbol, callerArgs)) + ((envSymbol, callerEnv))

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
      case symbol :: value :: Nil => (value, env + ((symbol.asInstanceOf[Symbol], value)))
      case _ => sys.error("define expected 2 arguments, got: " + args + " instead")
    }

  def load(args: List[Any], env: Environment): (Any, Environment) = {
    require(args.length == 1) // todo map over, and load a list of strings

    loadFromFile(args.head.asInstanceOf[String], env)
  }

  // helper functions

  def resolve(in: Any, env: Environment): (Any, Environment) = in match {
    case l: List[_] => evalList(l, env)
    case s: Symbol => (env(s), env)
    case x => (x, env)
  }

  def evalList(in: List[Any], env: Environment): (Any, Environment) =
    in.head match {
      case l: List[_] => evalList(l, env)
      case s: Symbol => env(s).asInstanceOf[Procedure](in.tail, env)
      case _ => sys.error("Can't eval a list that doesn't start with a symbol")
    }

  def loadFromFile(file: String, env: Environment): (Any, Environment) = {

    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()

    ((), Reader.parse(lines).foldLeft(env)((e: Environment, l: Any) => resolve(l, e)._2))
  }

  def strict(args: List[Any], env: Environment)(f: List[Any] => Any) =
    (f(args.map(x => resolve(x, env)._1)), env)

  // builtin functions

  def addition(a: List[Any], env: Environment) = strict(a, env) {
    (args: List[Any]) => args.foldLeft(0) { (a, b) => a + b.asInstanceOf[Int] }
  }

  def printProc(a: List[Any], env: Environment) = strict(a, env) {
    (args: List[Any]) => print(args.mkString(" ") + "\n")
  }

  def subtract(a: List[Any], env: Environment) = strict(a, env) {
    (args: List[Any]) => args.head.asInstanceOf[Int] - args.tail.foldLeft(0)((a, b) => a + b.asInstanceOf[Int])
  }

  def head(a: List[Any], env: Environment) = strict(a, env) {
    (args: List[Any]) => args.map(_.asInstanceOf[List[_]].head)
  }

  def tail(a: List[Any], env: Environment) = strict(a, env) {
    (args: List[Any]) => args.map(_.asInstanceOf[List[_]].tail)
  }
}
