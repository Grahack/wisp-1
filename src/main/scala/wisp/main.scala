package wisp

import jline.console.ConsoleReader
import jline.console.completer.Completer
import Interpretter._

object Main {

  var continue = true;

  var env: Environment = null

  def main(args: Array[String]) {

    val console = new ConsoleReader
    console.addCompleter(WispCompleter)

    reload(null, null) // sets the env

    while (continue) {
      val line = console.readLine("~> ")

      if (line == null)
        return ;

      if (!isBlank(line)) {

        try {
          val exprs = Reader(line)
          require(exprs.length == 1)
          val ast = exprs.head

          val (r, t) = time(eval(env, ast))

          val summary = Interpretter.summary(r)
          val info = "[Took " + t + "ms]"

          val spaces = console.getTerminal.getWidth - summary.length - info.length

          console.print(summary)
          if (spaces > 0)
            console.print(" " * spaces)
          else
            console.println()
          console.println(info)
          console.flush()
        }/* catch {
          case e => println("Caught exception: " + e)
        }*/
      }
    }

    def reload: Function = new Function {
      def apply(ignore: Environment, ignore2: List[Any]): Environment = {
        val q =
          args.foldLeft(new Environment()) {
            (e: Environment, file: String) => foldEval(e, Reader(Interpretter.read(file)))
          } + (exit.name -> exit) + (name -> reload)

        env = q.asInstanceOf[Environment]
        env
      }
      def name = Symbol(":reload")
    }
  }

  def exit = new Function {
    def name = Symbol(":exit")
    def apply(env: Environment, args: List[Any]) = {
      continue = false
      List()
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

      // TODO: this is hacky, and eventually should be removed
      if (before.startsWith("#")) {
        Builtin.values.map(f => {
          val n = f._1.name
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