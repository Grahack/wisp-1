package wisp

object Dag {
  def apply[A, P]() = new Dag[A, P](Map(), Map(), Map())
}

class Dag[A, P](val direct: Map[A, Set[A]], val reverse: Map[A, Set[A]], val payload: Map[A, P]) {

  def addValueTo(key: A, value: A, to: Map[A, Set[A]]): Map[A, Set[A]] = to + (key -> (to.getOrElse(key, Set()) + value))
  def removeValueFrom(key: A, value: A, from: Map[A, Set[A]]): Map[A, Set[A]] = {
    val newSet = (from(key) - value)
    if (newSet.isEmpty)
      from - key
    else
      from + (key -> newSet)
  }

  def addEdge(from: A, to: A) = {
    new Dag(addValueTo(from, to, direct), addValueTo(to, from, reverse), payload)
  }
  def removeEdge(from: A, to: A) = {
    new Dag(removeValueFrom(from, to, direct), removeValueFrom(to, from, reverse), payload)
  }

  def add(from: A, data: P, to: Set[A]) = {
    assert(!payload.contains(from))

    val newPayload = payload + (from -> data)

    to.foldLeft(new Dag(direct, reverse, newPayload)) {
      (a, b) => a.addEdge(from, b)
    }
  }

  def ancestors(of: A): Set[A] = {
    def run(seen: Set[A], of: A): Set[A] = {
      reverse.getOrElse(of, Set()).foldLeft(seen + of) { (a, b) =>
        if (!a.contains(b)) {
          run(a, b)
        } else a
      }
    }
    run(Set(), of)
  }

  def remove(key: A) = {
    // should I check it exists?
    val to = direct(key)

    val newReverse = to.foldLeft(reverse) {
      (rev, t) =>
        removeValueFrom(t, key, rev)
    }

    new Dag(direct - key, newReverse, payload - key)
  }

  def topologicalSort: Seq[A] = {
    // transliterated from wikipedia
    var graph = this
    var L = Seq[A]()
    var S = for (p <- graph.payload.keys.toSeq if !graph.reverse.contains(p)) yield p

    while (S.nonEmpty) {
      val n = S.head
      S = S.tail
      L = L :+ n
      for (m <- graph.direct.getOrElse(n, Set())) {
        graph = graph.removeEdge(n, m)
        if (!graph.reverse.contains(m))
          S = S :+ m
      }
    }
    if (graph.direct.nonEmpty || graph.reverse.nonEmpty)
      sys.error("Dag must have at least one cycle: " + graph.direct + " and reverse: " + graph.reverse)
    else
      L
  }
  
  def root: A = topologicalSort.head // TODO: this should be done a bit smarter...

  def toAscii = {
    import com.github.mdr.ascii.layout._

    val vertices = payload.keys.toList
    val edges = for (
      pair <- direct.toList;
      to <- pair._2
    ) yield (pair._1 -> to)

    Layouter.renderGraph(Graph(vertices, edges))
  }
}