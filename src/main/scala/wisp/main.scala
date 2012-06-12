package wisp

import scala.util.parsing.combinator.Parsers

object Main {

  def main(args: Array[String]) {
    println("Welcome to Wisp! Enter a blank line to evaluate. ")

    var env = Environment()

    while (true) {
      try {
        var line = new String()

        print("~> ")

        var break = false;
        while (!break) {
          val l = readLine()

          line = line + "\n" + l

          if (l.isEmpty)
            break = true
        }

        val p = Reader.parse(line) // hack to get rid of leading \t

        p.foreach { cell =>
          println(Eval.onAtom(cell, env))
        }

      } catch {
        case e => println("Error, caught exception: " + e)
      }
    }
  }
}