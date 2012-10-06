package wisp

import scalaz.IndSeq

object +: {
  def unapply(t: Vect): Option[(Any, Vect)] = if (t.isEmpty) None else Some(t.head -> t.tail)
}

object Vect {
  def apply(values: Any*) = new Vect(IndSeq(values))
  def fromSeq(values: Seq[Any]) = new Vect(IndSeq.fromSeq(values))
  def unapplySeq(x: Vect) = List.unapplySeq(x.data.self.toList) // this can probably be written much better
}

class Vect(val data: IndSeq[Any]) {

  def apply(index: Int) = data(index)
  def ++(other: Vect) = new Vect(data ++ other.data)

  def head: Any = data.self.head
  def tail: Vect = new Vect(data.tail)
  def last: Any = data.self.last
  def init: Vect = new Vect(data.init)
  
  def get(index: Int): Any = data(index)
  def set(index: Int, value: Any) = {
   // val q = wrapRope(data).slice(1, 10)
  }
  
  //def slice(from: Int, to: Int): Vect = new Vect(data.slice(from, to))

  def +:(v: Any) = new Vect(v +: data)
  def :+(v: Any) = new Vect(data :+ v)

  def isEmpty = data.self.isEmpty
  def length = data.self.measure


  override def equals(x: Any): Boolean = {
    x match {
      case s: String => equals(s.toList)
      case i: Iterable[_] => crawlCompare(data.self.toList, i)
      case v: Vect => crawlCompare(data.self.toList, v.data.self.toList)
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

  def map(f: Any => Any): Vect = {
    // Is there no better way?
//    val arr: Array[Any] = new Array(data.size)
//    
//    var i = 0
//    val t = data.foreach { x =>
//      arr(i) = f(x)
//      i = i + i
//    }
//    
//    new Vect(Rope.fromArray(arr))
    
    null
  }

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