package espringe.wisp

object SourceInfo {
  implicit val unknownSource = UnknownSource
}

sealed abstract trait SourceInfo {
  def print: String
}
object UnknownSource extends SourceInfo {
  def print = "Source: Unknown"
}
object NilSource extends SourceInfo {
  def print = "Nil is a singleton"
}
case class LexicalSource(file: String, column: Long, line: Long) extends SourceInfo {
  def print = s"Source: $file column $column line $line"
}
case class ComputedSource(from: W) extends SourceInfo {
  def print = "Computed from: \n\t" + from.toString.replaceAll("\n", "\n\t")
}
