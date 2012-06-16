package wisp

import scala.collection.immutable.HashMap
import jline.console.ConsoleReader

object Main {

  var continue = true;

  def main(args: Array[String]) {
    val console = new ConsoleReader

    var counter = 0
    var env = new HashMap[Any, Any]() + (Symbol(":exit") -> exit)

    while (continue) {
      val line = console.readLine("~> ")

      if (line == null)
        return ;

      if (!isBlank(line)) {

        try {
          val exprs = Reader(line)
          require(exprs.length == 1)
          val ast = exprs.head

          val (r, t) = time(Interpretter.resolveWithEnv(ast, env))

          counter = counter + 1

          val nextV = Symbol(":ret" + counter)
          val nextE = Symbol(":env" + counter)
          env = r._2 + (nextV -> r._1) + (nextE -> r._2)

          val summary = nextV.name + " = " + Interpretter.summary(r._1)
          val info = "[Took " + t + "ms]"

          val spaces = console.getTerminal.getWidth - summary.length - info.length

          console.print(summary)
          if (spaces > 0)
            console.print(" " * spaces)
          else
            console.println()
          console.print(info)
          console.println()
          console.flush()

        } catch {
          case x => console.println("Caught error: " + x)
        }
      }
    }
  }

  val exit = new BuiltinFunction {
    def name = Symbol(":exit")
    def apply(args: List[Any], env: HashMap[Any, Any]) = {
      continue = false;
      (1, env)
    }
  }

  def isBlank(line: String): Boolean = {
    return line.isEmpty // TODO: 'or all whitespace'..
  }

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    (ret, (System.nanoTime - s) / 1e6)
  }

}