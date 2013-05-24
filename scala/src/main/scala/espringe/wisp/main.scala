package espringe.wisp

object Main {

  def main(args: Array[String]) {
    require(args.size == 1, "Expected a single argument, the file to run")

    val file = new java.io.File(args.head)
    val dir = file.getParentFile()
    require(dir != null, s"Could not get parent dir for $file")

    val interpretter = new Interpretter(dir)

    Parser(scala.io.Source.fromFile(file))
      .map { interpretter(_) }
      .foreach { println(_) }

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

