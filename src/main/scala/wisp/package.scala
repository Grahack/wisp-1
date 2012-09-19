import scalaz.FingerTree
package object wisp {

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }

    
  // Primitives
  object If
  object Vau
  
  // Quote can trivially written as (#vau a _ (#vect-nth a 0)) but it seems a little nicer
  // having the reader emit this
  object Quote

}