package wisp

object Main {

  def timeFunc[A](fn: => A) = {
    val s = System.nanoTime
    val ret = fn
    val f = System.nanoTime

    val d = System.nanoTime - s

    val format = new java.text.DecimalFormat("#.##")

    val time = (f - s) match {
      case ns if ns < 1000 => ns + "ns"
      case µs if µs < 1e6 => format.format(µs / 1e3) + "µs"
      case ms if ms < 1e9 => format.format(ms / 1e6) + "ms"
      case s => format.format(s / 1e9) + "s"
    }

    (ret, time)
  }
}

