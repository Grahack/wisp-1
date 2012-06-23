package wisp

import Interpretter._

object Interpretter {

  import scala.annotation.tailrec

  def eval(env: Environment, in: Any): (Environment, Any) = in match {
    case l: List[_] => eval(env, l.head)._2.asInstanceOf[BuiltinFunction](env, l.tail) // TODO: thread the env?
    case s: Symbol => eval(env, env(s))
    case x => (env, x)
  }
  
  def seval(env: Environment, in: Any) = eval(env, in)._2

  def resolve(env: Environment, in: Any): Any = in match {
    case s: Symbol => env(s)
    case x => x
  }

  def evalBlock(env: Environment, in: List[Any]): (Environment, Any) = {
    in.foldLeft(env, List(): Any) {
      (pre, value) => eval(pre._1, value)
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
