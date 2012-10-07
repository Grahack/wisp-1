package object wisp {

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }

  val WList = Stream
  type WList = Stream[Any]
  type IsWList = Stream[_]

  object If
  object Vau


}
