package object wisp {
  type WMap = scala.collection.immutable.HashMap[Any, Any]

  val emptyWMap = scala.collection.immutable.HashMap[Any, Any]()

  type IsWMap = scala.collection.immutable.HashMap[_, _]

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }
  
  

}