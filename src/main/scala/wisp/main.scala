package wisp

object Main {
  def main(args: Array[String]) {

    require(args.size == 1, "Expected a single argument, name of file to load")

    val (file, loadTime) = timeFunc(load(args.head))

    println("Took " + loadTime + "ms to load file")

    val (parsed, parseTime) = timeFunc(Reader(file))

    println("Took " + parseTime + "ms to parse")

    val (res, time) = timeFunc(Interpretter(parsed))
    println("Took " + time + "ms to run")
    
    println("Result: " + res)
  }

  def timeFunc[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    (ret, (System.nanoTime - s) / 1e6)
  }
}