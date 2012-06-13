package wisp

object Environment {
  import Interpretter._

  def apply() =
    new Environment(Map[Symbol, Any](
      'vau -> vau _,
      'eval -> eval _,
      'define -> define _,
      'if -> ifProcedure _,
      // normal funcs
      // list stuff
  //    'head -> Head,
  //    'tail -> Tail,
      // io
 //     'print -> Print,
      // math
 //     '- -> Subtract,
      '+ -> addition _,
      // misc */
      'author -> "Eric Springer"))

}

class Environment(val map: Map[Symbol, Any]) {
  def apply(s: Symbol): Any = map(s)
  def +(s: Symbol, value: Any): Environment = new Environment((map + ((s, value))))
  def ++(env: Environment): Environment = new Environment(map ++ env.map)
}