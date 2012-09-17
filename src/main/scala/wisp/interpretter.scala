package wisp

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths

  def apply(form: Any, e: Dict): Any = eval(form, e)

  object WTypes extends Enumeration {
    type WType = Value
    val TypeBool, TypeSym, TypeNum, TypeDict, TypeStr, TypeVect, TypeType = Value
  }
  import WTypes._

  object WFunc extends Enumeration {
    type WFunc = Value

    val Eval = Value // primitive (ish)
    val TypeEq, TypeOf = Value
    val NumAdd, NumDiv, NumGreaterThan, NumGreaterThanOrEqual, NumEq, NumNeq, NumLessThan, NumLessThanOrEqual, NumMult, NumSub, NumToString = Value
    val StrCharAt, StrConcat, StrEq, StrIndexOf, StrLastIndexOf, StrLength, StrSlice, StrSplit, StrToSym, StrToVect = Value
    val SymToString, SymEq = Value
    val VectAppend, VectCons, VectLength, VectNth, VectReduce, VectSlice = Value
    val DictContains, DictGet, DictInsert, DictRemove, DictSize, DictToVect = Value
    val BoolNot, BoolEq = Value
    val Trace, Error = Value // debuggy
  }
  import WFunc._

  def eval(form: Any, e: Dict): Any = {

    form match {
      case s: Symbol => e(s)
      case f +: rawArgs => {
        eval(f, e) match {
          case VauRun(capEnv, argS, envS, capCode) => eval(capCode, capEnv + (argS -> rawArgs) + (envS -> e))
          case Vau => rawArgs match {
            case Vect(argS: Symbol, envS: Symbol, code) =>
              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
              require(!e.contains(envS), "Can't use symbol " + envS + " for binding an environment, as it already exists")
              require(envS != argS, "Can't use the same symbol for binding the environment and argument")
              VauRun(e, argS, envS, code)
            case x => sys.error("#vau expects three arguments, an arg symbol, an env symbol, and the body of the code. Instead found: " + rawArgs)
          }
          case If => {
            require(rawArgs.length == 3, "If statement require three arguments (cond, trueCase, false), was given: " + rawArgs)
            val cond = eval(rawArgs(0), e)
            require(cond.isInstanceOf[Boolean], "Condition in #if statement, should evalute to a boolean -- but instead got: " + cond + ". If statement was: " + rawArgs)
            if (cond.asInstanceOf[Boolean]) eval(rawArgs(1), e) else eval(rawArgs(2), e)
          }
          case wf: WFunc => (rawArgs.map(eval(_, e)).cons(wf)) match {
            case Vect(Eval, v, env: Dict) => eval(v, env)
            // type stuff
            case Vect(TypeEq, a: WType, b: WType) => a == b
            case Vect(TypeOf, a) => a match {
              case _: Boolean => TypeBool
              case _: Int => TypeNum
              case _: String => TypeStr
              case _: Symbol => TypeSym
              case _: Vect => TypeVect
              case _: Dict => TypeDict
              case _: WType => TypeType
            }
            case Vect(NumAdd, a: Int, b: Int) => a + b
            case Vect(NumDiv, a: Int, b: Int) => a / b
            case Vect(NumEq, a: Int, b: Int) => a == b
            case Vect(NumNeq, a: Int, b: Int) => a != b
            case Vect(NumGreaterThan, a: Int, b: Int) => a > b
            case Vect(NumGreaterThanOrEqual, a: Int, b: Int) => a >= b
            case Vect(NumLessThan, a: Int, b: Int) => a < b
            case Vect(NumLessThanOrEqual, a: Int, b: Int) => a <= b
            case Vect(NumMult, a: Int, b: Int) => a * b
            case Vect(NumSub, a: Int, b: Int) => a - b
            case Vect(NumToString, a: Int) => a.toString()
            case Vect(StrCharAt, str: String, at: Int) => str.charAt(at).toString
            case Vect(StrConcat, a: String, b: String) => a + b
            case Vect(StrEq, a: String, b: String) => a == b
            case Vect(StrIndexOf, str: String, search: String, startIndex: Int) => str.indexOf(search, startIndex)
            case Vect(StrLastIndexOf, str: String, search: String, lastIndex: Int) => str.lastIndexOf(search, lastIndex)
            case Vect(StrLength, str: String) => str.length
            case Vect(StrSlice, str: String, from: Int, until: Int) => str.slice(from, until)
            case Vect(StrSplit, str: String, using: String) => Vect(str.split(using): _*) // TODO: careful, is this using regex?
            case Vect(StrToSym, str: String) => Symbol(str)
            case Vect(StrToVect, str: String) => Vect(str.toCharArray().map(x => x.toString): _*)
            case Vect(SymToString, sym: Symbol) => sym.name
            case Vect(SymEq, a: Symbol, b: Symbol) => a == b
            case Vect(VectAppend, vect: Vect, v) => vect.append(v)
            case Vect(VectCons, vect: Vect, v) => vect.cons(v)
            case Vect(VectSlice, vect: Vect, from: Int, until: Int) => vect.slice(from, until)
            case Vect(VectNth, vect: Vect, index: Int) => vect(index)
            case Vect(VectReduce, vect: Vect, func) => eval(vect.reduce((a, b) => Vect(func, a, b)), e) // TODO: probably broken?
            case Vect(DictContains, dict: Dict, k) => dict.contains(k)
            case Vect(DictGet, dict: Dict, k) => dict(k)
            case Vect(DictInsert, dict: Dict, k, v) => dict + (k -> v)
            case Vect(DictRemove, dict: Dict, k) => dict - k
            case Vect(DictSize, dict: Dict) => dict.size
            case Vect(DictToVect, a: Dict) => a.data.foldLeft(Vect()) { (p, n) => p.append(n) }
            case Vect(BoolNot, arg: Boolean) => !arg
            case Vect(BoolEq, a: Boolean, b: Boolean) => a == b
            case Vect(Error) => sys.error("Code called an error")
            case Vect(Error, msg: String) => sys.error("Code called an errror with msg: " + msg)
            case Trace +: args => println(args.mkString)
            case Vect(VectLength, vec: Vect) => vec.length
            case x => sys.error("Unexpected arguments with: " + x)
          }

          case x => sys.error("When evaluating a vect, the first argument was an unexpected: " + x)
        }
      }
      case x => x
    }
  }

  // Primitives
  object If
  object Vau

  case class VauRun(capEnv: Dict, argS: Symbol, envS: Symbol, capCode: Any) {
    override def toString = "$vau$"
  }

  def startingEnv = Dict() +
    // Some pretty primitive stuff
    (Symbol("#eval") -> Eval) +
    (Symbol("#if") -> If) +
    (Symbol("#vau") -> Vau) +
    // Types
    (Symbol("#Bool") -> TypeBool) +
    (Symbol("#Dict") -> TypeDict) +
    (Symbol("#Num") -> TypeNum) +
    (Symbol("#Str") -> TypeStr) +
    (Symbol("#Sym") -> TypeSym) +
    (Symbol("#Type") -> TypeType) +
    (Symbol("#type-eq") -> TypeEq) +
    (Symbol("#type-of") -> TypeOf) +
    (Symbol("#Vect") -> TypeVect) +
    // some num stuff
    (Symbol("#num-add") -> NumAdd) +
    (Symbol("#num-div") -> NumDiv) +
    (Symbol("#num-eq") -> NumEq) +
    (Symbol("#num-gt") -> NumGreaterThan) +
    (Symbol("#num-gte") -> NumGreaterThanOrEqual) +
    (Symbol("#num-lt") -> NumLessThan) +
    (Symbol("#num-lte") -> NumLessThanOrEqual) +
    (Symbol("#num-mult") -> NumMult) +
    (Symbol("#num-neq") -> NumNeq) +
    (Symbol("#num-sub") -> NumSub) +
    (Symbol("#num-to-str") -> NumToString) +
    // string stuff
    (Symbol("#str-chat-at") -> StrCharAt) +
    (Symbol("#str-concat") -> StrConcat) +
    (Symbol("#str-eq") -> StrEq) +
    (Symbol("#str-index-of") -> StrIndexOf) +
    (Symbol("#str-last-index-of") -> StrLastIndexOf) +
    (Symbol("#str-length") -> StrLength) +
    (Symbol("#str-slice") -> StrSlice) +
    (Symbol("#str-split") -> StrSplit) +
    (Symbol("#str-to-sym") -> StrToSym) +
    (Symbol("#str-to-vect") -> StrToVect) +
    // sym stuff
    (Symbol("#sym-to-string") -> SymToString) +
    (Symbol("#sym-eq") -> SymEq) +
    // vect functions
    (Symbol("#vect-append") -> VectAppend) +
    (Symbol("#vect-cons") -> VectCons) +
    (Symbol("#vect-length") -> VectLength) +
    (Symbol("#vect-nth") -> VectNth) +
    (Symbol("#vect-reduce") -> VectReduce) +
    (Symbol("#vect-slice") -> VectSlice) +
    // Dict functions
    (Symbol("#dict-contains") -> DictContains) +
    (Symbol("#dict-empty") -> Dict()) +
    (Symbol("#dict-get") -> DictGet) +
    (Symbol("#dict-insert") -> DictInsert) +
    (Symbol("#dict-remove") -> DictRemove) +
    (Symbol("#dict-size") -> DictSize) +
    (Symbol("#dict-to-vect") -> DictToVect) +
    // boolean
    (Symbol("#bool-eq") -> BoolEq) +
    (Symbol("#bool-false") -> false) +
    (Symbol("#bool-not") -> BoolNot) +
    (Symbol("#bool-true") -> true) +
    // debug
    (Symbol("#error") -> Error) +
    (Symbol("#trace") -> Trace)
}
