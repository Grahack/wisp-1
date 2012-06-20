package wisp

object Interpretter {

  def eval(env: Environment, in: Any): Any =
    in match {
      case l: List[_] =>
        eval(env, l.head).asInstanceOf[Function](env, l.tail)
      case s: Symbol => eval(env, env(s))
      case x => x
    }

  def resolve(env: Environment, in: Any): Any =
    in match {
      case s: Symbol => env(s)
      case x => x
    }

  def foldEval(start: Environment, in: List[Any]): Environment = {
    in.foldLeft(start) {
      (env, value) => eval(env, value).asInstanceOf[Environment]
    }
  }

  def format(a: Any): String = {
    a match {
      case l: List[_] => l.map(format(_)).mkString("(", " ", ")")
      case s: Symbol => s.name
      case i: Int => i.toString
      case m: Map[_, _] => "(#map " + m.toList.map(x => "(" + format(x._1) + " " + format(x._2) + ")").mkString(" ") + ")"
      case b: Function => b.name.name
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
