package object wisp {

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }
  
  val WList = Stream
  type WList = Stream[Any]
  type IsWList = Stream[_]

  object If
  object Vau
  
  object WTypes extends Enumeration {
    type WType = Value
    val TypeBool, TypeSym, TypeNum, TypeDict, TypeFunc, TypeList, TypeType = Value
  }

  object WFunc extends Enumeration {
    type WFunc = Value

    val Eval = Value // primitive (ish)
    val TypeEq, TypeOf = Value
    val NumAdd, NumDiv, NumGreaterThan, NumGreaterThanOrEqual, NumEq, NumNeq, NumLessThan, NumLessThanOrEqual, NumMult, NumSub, NumToList = Value
    val SymEq, SymToVect = Value
    val ListCons, ListEmpty, ListLength, ListMake, ListRest, ListNth, ListReduce = Value // TODO: should only have: cons, rest, empty
    val DictContains, DictGet, DictInsert, DictRemove, DictSize, DictToList = Value
    val BoolNot, BoolEq = Value
    val Trace, Error = Value // debuggy
  }
}
