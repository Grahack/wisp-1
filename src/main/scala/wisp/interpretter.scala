package wisp

import Interpretter._

object Interpretter {

  import scala.annotation.tailrec

  def eval(in: Any, env: Environment): (Any, Environment) = in match {
    case l: List[_] => eval(eval(l.head, env)._1, env)._1.asInstanceOf[BuiltinFunction](l.tail, env) // TODO: thread the env? 
    case x => (resolve(x, env), env)
  }

  def resolve(in: Any, env: Environment): Any = in match {
    case s: Symbol => env(s)
    case x => x
  }

  def evalBlock(in: List[Any], env: Environment): (Any, Environment) = {
    in.foldLeft((List(): Any, env)) {
      (pre, value) => eval(value, pre._2)
    }
  }

  def format(a: Any): String = {
    a match {
      case l: List[_] => l.map(format(_)).mkString("(", " ", ")")
      case s: Symbol => s.name
      case i: Int => i.toString
      case m: Map[_, _] => "(#map " + m.toList.map(x => "(" + format(x._1) + " " + format(x._2) + ")").mkString(" ") + ")"
      case b: BuiltinFunction => b.name.name
      case s: String => '"' + s + '"'
      case b: Boolean => if (b) "#true" else "#false"
      case _ => sys.error("Unknown type of: " + a)
    }
  }

  def summary(v: Any): String = {
    val r = format(v)
    if (r.length > 200) r.substring(0, 197) + "..." else r
  }

  def read(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }

}
