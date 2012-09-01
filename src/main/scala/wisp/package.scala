package object wisp {

  type Env = scala.collection.immutable.HashMap[Symbol, Any]

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }
  

}