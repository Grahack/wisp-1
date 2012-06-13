package wisp

import scala.util.parsing.combinator.Parsers

object Main {

  def main(args: Array[String]) {
    println("Welcome to Wisp! Enter a blank line to evaluate. ")

    var env = Environment()
    var counter = 0

    while (true) {
      try {
        var line: String = ""

        var continue = true
        var indent = 0

        while (continue) {
          print("~> " + "\t" * indent)
          val l = readLine()

          line = line + "\n" + ("\t" * indent) + (
            if (l.endsWith("\\")) {
              indent = indent + 1
              l.dropRight(1)
            } else if (l.isEmpty) {
              indent = indent - 1
              l
            } else {
              l
            })

          if (indent <= 0)
            continue = false
        }

        val p = Reader.parse(line)

        p.foreach { cell =>
          {
            val r = Interpretter.resolve(cell, env)

            counter = counter + 1

            val nextV = Symbol(":v" + counter)
            val nextE = Symbol(":e" + counter)
            env = r._2 + ((nextV, r._1)) + ((nextE, r._2))

            println(nextV.toString + " = " + r._1)
          }
        }

      } catch {
        case e => println("Error, caught exception: " + e)
      }
    }
  }
}