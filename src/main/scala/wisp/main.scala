package wisp

object Main {

  def main(args: Array[String]) {
    println("Welcome to Wisp!")

    // var env = Environment()

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

        println(p)

        //
        //        p.foreach { cell =>
        //          println(Interpretter.eval(cell, env))
        //        }

      } catch {
        case e => println("Error, caught exception: " + e)
      }
    }
  }
}