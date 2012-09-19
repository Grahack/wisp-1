package wisp

object +: {
  def unapply(t: Vect): Option[(Any, Vect)] = if (t.isEmpty) None else Some(t.head -> t.tail)
}

object Vect {
  def apply(values: Any*) = new Vect(scalaz.IndSeq(values: _*))
  def fromSeq(values: Seq[Any]) = new Vect(scalaz.IndSeq.fromSeq(values))
  def unapplySeq(x: Vect) = List.unapplySeq(x.data.self.toList) // TODO: a fast way?
}

class Vect(val data: scalaz.IndSeq[Any]) {

  def apply(index: Int) = data(index)
  def ++(other: Vect) = new Vect(data ++ other.data)

  def head: Any = data.self.head
  def tail: Vect = new Vect(data.tail)
  def last: Any = data.self.last
  def init: Vect = new Vect(data.init)

  def +:(v: Any): Any = new Vect(v +: data)
  def :+(v: Any) = new Vect(data :+ v)

  def isEmpty = data.self.isEmpty
  def nonEmpty = !isEmpty

  def length = data.self.measure // TODO: check if this is correct?

  // def reduce(op: (Any, Any) => Any) = data.reduce(op)

  // don't expose to the interpretter:

  override def equals(x: Any): Boolean = {

    if (!x.isInstanceOf[Vect]) return false;

    val aIt = data.self.iterator
    val bIt = x.asInstanceOf[Vect].data.self.iterator

    while (aIt.hasNext && bIt.hasNext) {
      if (aIt.next() != bIt.next()) return false;
    }

    aIt.hasNext == bIt.hasNext
  }

  def map(f: Any => Any) = new Vect(data.map(f))

  def foreach(f: Any => Unit) = data.self.foreach(f)

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

  override def toString() = {
    val sb = StringBuilder.newBuilder

    sb += '['

    data.self.foreach { x =>
      sb ++= x.toString
    }

    sb += ']'

    sb.toString
  }

}