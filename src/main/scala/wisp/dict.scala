package wisp

// So we don't have to deal with stupid type-erasure all the time

object Dict {
  def apply() = new Dict(scala.collection.immutable.HashMap[Any, Any]())
}

class Dict(val data: scala.collection.immutable.HashMap[Any, Any]) {

  def +(elem: (Any, Any)): Dict = {
    require(!data.contains(elem._1), "Could not add: " + elem + " to dict, as key already exists (with value: " + data(elem._1) + ")")
    new Dict(data + elem)
  }

  def -(key: Any): Dict = {
    require(data.contains(key))
    new Dict(data - key)
  }

  def merge(r: Dict): Dict = {
    // TODO: some smart checking or something
    new Dict(data ++ r.data)
  }

  def foreach(f: ((Any, Any)) => Unit) = data.foreach(f)

  def contains(k: Any) = data.contains(k)

  def apply(k: Any) = data(k)

  def get(k: Any) = data.get(k)

  def size = data.size

  override def toString = "{Dict. size: " + size + " }"
}