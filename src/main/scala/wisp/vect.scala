package wisp

object +: {
  def unapply(t: Vect): Option[(Any, Vect)] = if (t.isEmpty) None else Some(t.head -> t.tail)
}

object Vect {
  def apply(values: Any*) = new Vect(Vector(values: _*))
  def fromSeq(values: Seq[Any]) = new Vect(Vector(values: _*))
  def unapplySeq(x: Vect) = Vector.unapplySeq(x.data)
}

class Vect(val data: Vector[Any]) {

  def apply(index: Int) = data(index)
  def ++(other: Vect) = new Vect(data ++ other.data)

  def head: Any = data.head
  def tail: Vect = new Vect(data.tail)
  def last: Any = data.last
  def init: Vect = new Vect(data.init)

  def +:(v: Any) = new Vect(v +: data)
  def :+(v: Any) = new Vect(data :+ v)

  def isEmpty = data.isEmpty
  def length = data.length


  override def equals(x: Any): Boolean = {
    x match {
      case s: String => equals(s.toList)
      case i: Iterable[_] => crawlCompare(data, i)
      case v: Vect => crawlCompare(data, v.data)
      case x => false
    }
  }

  private def crawlCompare(x: Iterable[Any], y: Iterable[Any]): Boolean = {
    val aIt = x.iterator
    val bIt = y.iterator

    while (aIt.hasNext && bIt.hasNext) {
      if (aIt.next() != bIt.next()) return false;
    }

    true
  }

  def map(f: Any => Any) = new Vect(data.map(f))

  def convertToString: Option[String] = {

    val sb = StringBuilder.newBuilder

    data.foreach {
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
      data.foreach { x =>
        sb ++= x.toString
      }
      sb += ']'

      sb.toString
    }
}