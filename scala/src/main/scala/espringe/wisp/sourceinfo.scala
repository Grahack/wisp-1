package espringe.wisp

object SourceInfo {
  implicit val unknownSource = new SourceInfo {
    override def print = "Source: Unknown"
  }
}

trait SourceInfo {
  def print: String
}
object UnknownSource extends SourceInfo {
  def print = "Source: Unknown"
}

case class LexicalSource(file: String, column: Long, line: Long) extends SourceInfo {
  def print = s"Source: $file column $column line $line"
}
class ComputedSource(from: W) extends SourceInfo {
  def print = "Computed from: \n\t" + from.toString.replaceAll("\n", "\n\t")
}