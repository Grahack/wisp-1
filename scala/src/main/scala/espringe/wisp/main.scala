package espringe.wisp

object Main {

  def main(args: Array[String]) {
    require(args.size == 1, "Expected a single argument, the file to run")

    args.map(new java.io.File(_))
      .map(scala.io.Source.fromFile(_))
      .flatMap { Parser(_) }
      .map { Interpretter(_) }
      .foreach { println(_) }

  }

  def parseArgs(args: Seq[String]) = {

    val (toInterpret, toWatch) = args.partition(s => s == "--watch" || s == "-w")

    val toInterpretFiles = toInterpret.map { x =>
      if (x.startsWith("~")) x.tail else x
    }

    val toWatchFiles = (toInterpret.filter(_.startsWith("~")).map(_.tail) ++ toWatch)

    toInterpretFiles -> toWatchFiles
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

