package wisp

import scala.collection.immutable.HashMap
import jline.console.ConsoleReader
import jline.console.completer.Completer

object Main {

  var continue = true;

  var env = new HashMap[Any, Any]() + (exit.name -> exit)

  def main(args: Array[String]) {
    val console = new ConsoleReader
    console.addCompleter(WispCompleter)

    var counter = 0

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
          console.println(info)

        } catch {
          case x => console.println("Caught error: " + x)
        }
      }
    }
  }

  def exit = new BuiltinFunction {
    def name = Symbol(":exit")
    def apply(args: List[Any], env: HashMap[Any, Any]) = {
      continue = false
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

      // TODO: once a core.wisp is made, this block should be removed
      if (before.startsWith("#")) {
        Interpretter.builtinFunctions.map(f => {
          val n = f.name.name
          if (n.startsWith(before)) {
            results.add(n + " ")
          }
        })
      }

      env.map(e => {
        if (e._1.isInstanceOf[Symbol]) {
          val s = e._1.asInstanceOf[Symbol].name

          if (s.startsWith(before)) {
            results.add(s + " ") // TODO: might not want to add space if there's an unterminated paren?
          }
        }
      })

      start
    }

    def split(buffer: String, at: Int): (String, Int) = {
      val from = math.max(at - 1, 0)
      
      // TODO: probably a nicer way to write this then nested maxes
      val lio = math.max(buffer.lastIndexOf(' ', from),
          math.max(buffer.lastIndexOf('(', from),
              buffer.lastIndexOf(')', from)))
      val start = if (lio < at) lio + 1 else at
      val before = buffer.substring(math.min(start, buffer.length), at)

      (before, start)
    }

  }

}