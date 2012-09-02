package wisp

import java.nio.file.Paths

object Main {
  def main(args: Array[String]) {

    require(args.size == 1, "Expected a single argument, name of file to load")

    val (res, env) = Interpretter(Paths.get(args.head))

    println(res)
  }

  def timeFunc[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    (ret, (System.nanoTime - s) / 1e6)
  }
}