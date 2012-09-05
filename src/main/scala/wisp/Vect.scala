package wisp

object +: {
  def unapply(t: Vect): Option[(Any, Vect)] = t.headOption map (h => h -> t.tail)
}

object Vect {

  def apply(values: Any*) = new Vect(Vector(values: _*))
  
  def apply(value: List[Any]) = new Vect(Vector(value: _*))
  
  def unapplySeq(x: Vect) = Vector.unapplySeq(x.data) //.map(new Vect(_))
}

class Vect(val data: Vector[Any]) {

  def apply(index: Int) = data(index)

  def concat(other: Vect) = new Vect(data ++ other.data)

  def last: Any = data.last
  def head: Any = data.head

  def cons(v: Any): Any = new Vect(v +: data)
  def append(v: Any) = new Vect(data :+ v)

  def reverse = new Vect(data.reverse)

  def isEmpty = data.isEmpty
  def length = data.length

  def foldLeft[B](z: B)(op: (B, Any) => B): B = {
    data.foldLeft(z)(op)
  }

  // don't expose to the interpretter:

  def span(p: Any => Boolean): (Vect, Vect) = {
    val r = data.span(p)
    (new Vect(r._1), new Vect(r._2))
  }

  def partition(p: Any => Boolean): (Vect, Vect) = {
    val r = data.partition(p)
    (new Vect(r._1), new Vect(r._2))
  }

  def map(f: Any => Any) = new Vect(data.map(f))

  def reduce(f: (Any, Any) => Any) = data.reduce(f)
  
  def foreach(f: Any => Unit) = data.foreach(f)

  def headOption = data.headOption
  def tail = new Vect(data.tail)
  
  def mkString = data.mkString
  
  override def toString() = data.toString()

}