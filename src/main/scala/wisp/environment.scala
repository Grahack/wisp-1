package wisp

object Environment {

  def apply() =
    new Environment(Map[Symbol, Any](
      'vau -> Vau,
      'eval -> Eval,
      'define -> Define,
      'if -> If,
      // normal funcs
      Symbol(",") -> Id,
      // list stuff
      'head -> Head,
      'tail -> Tail,
      // io
      'print -> Print,
      // math
      '- -> Subtract,
      '+ -> Addition,
      // misc
      'author -> "Eric Springer"), None)

}

class Environment(var map: Map[Symbol, Any], val parent: Option[Environment]) {
  def get(s: Symbol): Any = map.get(s).orElse(parent.map(_.get(s))).getOrElse(sys.error("Was searching for symbol " + s + " but couldn't find it."))
  def set(s: Symbol, value: Any) = {
    map = map + ((s, value))
  }
  def copyOnto(onto: Option[Environment]): Environment = new Environment(map, parent.map(_.copyOnto(onto)).orElse(onto))
}