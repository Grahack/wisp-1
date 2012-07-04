package object wisp {
  type WMap = scala.collection.immutable.HashMap[Any, Any]
  type IsWMap = scala.collection.immutable.HashMap[_, _]
  
  type WEnv = WMap
  
  type WList = scala.collection.immutable.List[Any]
  
  type IsWList = scala.collection.immutable.List[_]
  
  trait WFunc {
    def apply(args: WList): Any
    def name: Symbol
    def err = sys.error("Fatal error in: " + name)
  }
}