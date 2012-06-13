package wisp

import scala.util.parsing.combinator.Parsers

object Main {

  def main(args: Array[String]) {
    println("Welcome to Wisp! Enter a blank line to evaluate. ")

    var env = Environment()

    var counter = 0

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

        val p = Reader.parse(line)

        p.foreach { cell =>
          {
            val r = Interpretter.resolve(cell, env)

            counter = counter + 1

            val nextV = Symbol(":v" + counter)
            val nextE = Symbol(":e" + counter)
            env = r._2 + (nextV, r._1) + (nextE, r._2)

            println(nextV.toString + " = " + r._1)
          }
        }

      } catch {
        case e => println("Error, caught exception: " + e)
      }
    }
  }
}