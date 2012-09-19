package wisp

object +: {
  def unapply(t: Vect): Option[(Any, Vect)] = if (t.isEmpty) None else Some(t.head -> t.tail)
}

object Vect {
  def apply(values: Any*) = new Vect(scalaz.IndSeq(values: _*))
  def fromSeq(values: Seq[Any]) = new Vect(scalaz.IndSeq.fromSeq(values))
  def unapplySeq(x: Vect) = List.unapplySeq(x.data.self.toList) // TODO: a fast way?
}

class Vect(val data: scalaz.IndSeq[Any]) extends Iterable[Any] {

  def iterator = data.self.iterator

  def apply(index: Int) = data(index)
  def ++(other: Vect) = new Vect(data ++ other.data)

  override def head: Any = data.self.head
  override def tail: Vect = new Vect(data.tail)
  override def last: Any = data.self.last
  override def init: Vect = new Vect(data.init)

  def +:(v: Any) = new Vect(v +: data)
  def :+(v: Any) = new Vect(data :+ v)

  override def isEmpty = data.self.isEmpty

  def length = data.self.measure

  // def reduce(op: (Any, Any) => Any) = data.reduce(op)

  // don't expose to the interpretter:

  override def equals(x: Any): Boolean = {
    x match {
      case s: String => equals(s.toList)
      case i: Iterable[_] =>
        val aIt = iterator
        val bIt = i.iterator

        while (aIt.hasNext && bIt.hasNext) {
          if (aIt.next() != bIt.next()) return false;
        }

        aIt.hasNext == bIt.hasNext
      case x => false
    }

  }
  def vmap(f: Any => Any) = new Vect(data.map(f))

  def convertToString: Option[String] = {

    val sb = StringBuilder.newBuilder

    data.self.foreach {
      c =>
        if (c.isInstanceOf[Char])
          sb += c.asInstanceOf[Char]
        else
          return None
    }

    Some(sb.toString)
  }

  override def toString() =

    convertToString.getOrElse {
      val sb = StringBuilder.newBuilder

      //if (headOption)

      sb += '['
      data.self.foreach { x =>
        sb ++= x.toString
      }
      sb += ']'

      sb.toString
    }
}