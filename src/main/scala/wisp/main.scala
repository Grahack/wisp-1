package wisp

import java.nio.file.Paths
import jline.console.ConsoleReader
import jline.console.completer.Completer

object Main {
  def main(args: Array[String]) {

    val console = new ConsoleReader

    require(args.size == 1, "Expected a single argument, the name of file to run")

    val path = args.head

    val (res, env) = Interpretter(Paths.get(path))

    console.addCompleter(new WispCompleter(env))

    var line: String = null

    while (true) {
      val line = console.readLine("~> ")

      if (line == null)
        return ;

      try {
        val processed = Reader(line).foreach { processed =>

          val (res, time) = timeFunc(Interpretter.eval(env, processed))

          val summary = "Result: " + res
          val info = "[Took " + time + "ms]"

          val spaces = console.getTerminal.getWidth - summary.length - info.length

          console.print(summary)
          if (spaces > 0)
            console.print(" " * spaces)
          else
            console.print("\n")
          console.println(info)
          console.flush()
        }

      } // catch { case x => console.println("Caught errror: " + x) }
    }

    println(res)
  }

  def timeFunc[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    (ret, (System.nanoTime - s) / 1e6)
  }
}

class WispCompleter(e: Env) extends Completer {

  def complete(buffer: String, at: Int, results: java.util.List[CharSequence]) = {

    val (before, start) = split(buffer, at)

    e.foreach(f => {
      val n = f._1.name
      if (n.startsWith(before)) {
        results.add(n + " ")
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