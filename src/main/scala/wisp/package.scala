package object wisp {

  def load(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }

  // Primitives
  object If
  object Vau

  object WTypes extends Enumeration {
    type WType = Value
    val TypeBool, TypeSym, TypeNum, TypeDict, TypeVect, TypeType = Value
  }

  object WFunc extends Enumeration {
    type WFunc = Value

    val Eval = Value // primitive (ish)
    val TypeEq, TypeOf = Value
    val NumAdd, NumDiv, NumGreaterThan, NumGreaterThanOrEqual, NumEq, NumNeq, NumLessThan, NumLessThanOrEqual, NumMult, NumSub, NumToVect = Value
    val SymEq, SymToVect = Value
    val VectAppend, VectCons, VectLength, VectMake, VectNth, VectReduce, VectSlice = Value
    val DictContains, DictGet, DictInsert, DictRemove, DictSize, DictToVect = Value
    val BoolNot, BoolEq = Value
    val Trace, Error = Value // debuggy
  }
}
