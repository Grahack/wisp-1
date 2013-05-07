package espringe.wisp

object Main {

  def main(args: Array[String]) {

    if (args.isEmpty)
      sys.error("Expect the name of the file(s) to interpret")

    val fileSources = args
      .map(new java.io.File(_))
      .map(scala.io.Source.fromFile(_))

    fileSources
      .flatMap { x =>
        val (forms, time) = timeFunc(Parser(x))
        println("Parsing took: " + time)
        forms.value
      }
      .foreach { x =>
        val (res, time) = timeFunc(Interpretter(x))
        println("Interpretting took: " + time)
        println("Result: " + res)
      }
  }
  def timeFunc[A](fn: => A) = {
    val s = System.nanoTime
    val ret = fn
    val f = System.nanoTime

    val d = System.nanoTime - s

    val format = new java.text.DecimalFormat("#.##")

    val time = (f - s) match {
      case ns if ns < 1000 => ns + "ns"
      case us if us < 1e6 => format.format(us / 1e3) + "us"
      case ms if ms < 1e9 => format.format(ms / 1e6) + "ms"
      case s => format.format(s / 1e9) + "s"
    }

    (ret, time)
  }
}

