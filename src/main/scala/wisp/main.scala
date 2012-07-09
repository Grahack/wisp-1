package wisp

import jline.console.ConsoleReader
import jline.console.completer.Completer
import Interpretter._

object Main {

  def main(args: Array[String]) {

    val console = new ConsoleReader
    console.addCompleter(WispCompleter)

    var expandVals = Interpretter.builtinValues

    def run(input: Any, newSymb: Symbol) = {
      try {
        val (r, t) = time(eval[Any](input))

        expandVals = expandVals + (newSymb -> r)

        val summary = newSymb.name + " = " + Interpretter.summary(r)
        val info = "[Took " + t + "ms]"

        val spaces = console.getTerminal.getWidth - summary.length - info.length

        console.print(summary)
        if (spaces > 0)
          console.print(" " * spaces)
        else
          console.println()
        console.println(info)
        console.flush()

      } /*catch {
          case x => console.println("Caught error: " + x)
        }*/

    }

    args.foreach {
      (file: String) => {
        val fileSummaryStart = math.max(0, file.lastIndexOf('/')+1)
        val fileSummaryEnd = math.min(file.size-1, file.lastIndexOf('.'))
        
        run(Reader(expandVals, read(file)), Symbol(":" + file.substring(fileSummaryStart, fileSummaryEnd)))        
      }
    }

    var count = 0
    var continue = true;
    while (continue) {
      val line = console.readLine("~> ")

      if (line == null)
        return ;

      if (!line.matches("^\\s*$")) { // if line contains non-whitespace
        count = count + 1
        
        run(Reader(expandVals, line), Symbol(":res" + count))
      }
    }
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
      if (before.isEmpty || before.startsWith("#")) {
        Interpretter.builtinValues.map(f => {
          val n = f._1.name
          if (n.startsWith(before)) {
            results.add(n + " ")
          }
        })
      }

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