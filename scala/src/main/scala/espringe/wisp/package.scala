package espringe.wisp

package object wisp {

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }


}
