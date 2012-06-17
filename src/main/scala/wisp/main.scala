package wisp

import scala.collection.immutable.HashMap
import jline.console.ConsoleReader
import jline.console.completer.Completer

object Main {

  var continue = true;
  var env = new HashMap[Any, Any]() + (Symbol(":exit") -> exit)

  def main(args: Array[String]) {
    val console = new ConsoleReader

    var counter = 0

    console.addCompleter(WispCompleter)

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
          //console.flush()

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

  object WispCompleter extends Completer {

    def complete(buffer: String, at: Int, results: java.util.List[CharSequence]) = {

      val (before, start) = split(buffer, at)

      env.map(e => {
        if (e._1.isInstanceOf[Symbol]) {
          val s = e._1.asInstanceOf[Symbol].name

          if (s.startsWith(before)) {
            results.add(s)
          }
        }
      })

      start
    }

    def split(buffer: String, at: Int): (String, Int) = {
      val lio = buffer.lastIndexOf(' ', math.max(at - 1, 0))
      val start = if (lio < at) lio + 1 else at
      val before = buffer.substring(math.min(start, buffer.length), at)

      (before, start)
    }

  }

}