package wisp

object Main {

  def main(args: Array[String]) {
    println("Welcome to Wisp!")

    var env = Environment()

    while (true) {
     try {
        print("~> ")
        val l = readLine()
        val p = Reader.parse(l)

        p.foreach { cell =>
          println(Interpretter.eval(cell, env))
        }

      } catch {
        case e => println("Error, caught exception: " + e)
      }
    }
  }
}