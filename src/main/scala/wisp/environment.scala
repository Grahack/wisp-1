package wisp

import scala.collection.mutable.Map

object GroundEnvironment extends Environment(Map(), List()) {

}

class Environment(
  private val lookup: Map[Symbol, Any],
  private val parents: List[Environment]) {

  // TODO: what's the functional way of writing a DFS?
  def get(s: Symbol): Option[Any] = {
    if (lookup.contains(s))
      return Some(lookup(s))
    else
      for (e <- parents) {
        val x = e.get(s)
        if (x.isDefined)
          return x
      }

    return None

  }
  
  def add(s: Symbol, v: Any) = lookup.put(s, v)

}