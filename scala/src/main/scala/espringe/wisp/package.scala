package espringe


package object wisp {
  
  type Dict = Map[W, W]

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }


}