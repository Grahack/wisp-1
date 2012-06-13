package wisp

object Environment {
  import Interpretter._

  def apply(): Map[Any, Any] =
    Map(
      // special forms
      'lambda -> lambda _,
      'eval -> eval _,
      'define -> define _,
      'if -> ifProcedure _,
      // list stuff
      'head -> head _,
      'tail -> tail _,
      // io
      'print -> printProc _,
      'load -> load _,
      // math
      '- -> subtract _,
      '+ -> addition _,
      // misc */
      'true -> true,
      'false -> false,
      'author -> "Eric Springer")

}