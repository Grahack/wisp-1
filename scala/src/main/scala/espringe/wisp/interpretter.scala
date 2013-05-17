package espringe.wisp

object Interpretter {

  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(new HashMap(), form)

  def eval(e: Dict, form: W): W = {

    object WEval {
      def unapply(value: W) = Some(eval(e, value))
    }
    object BoolEval {
      def unapply(value: W) = eval(e, value).asBool
    }
    object DictEval {
      def unapply(value: W) = eval(e, value).asDict
    }
    object NumEval {
      def unapply(value: W) = eval(e, value).asNum
    }
    object ListEval {
      def unapply(value: W) = eval(e, value).asList
    }
    object SymEval {
      def unapply(value: W) = eval(e, value).asSym
    }
    object TypeEval {
      def unapply(value: W) = eval(e, value).asType
    }
    object PairEval {
      def unapply(value: W) = eval(e, value) match {
        case a ~: b ~: WNil() => Option((a, b))
        case _ => None
      }
    }

    import BuiltinFunctionNames._

    form match {
      case s: Sym => {
        require(e.contains(s), s"Could not find $s in environment $e")
        e(s)
      }
      case fnCall @ WCons(WEval(fn), rawArgs, _) =>
        def from = new ComputedSource(fnCall)
        fn match { // in order to tail call if/eval, can't just dynamic-dispatch out
          case UDF(capEnv, argS, envS, capCode, _) =>
            eval(capEnv + (Sym(argS) -> rawArgs) + (Sym(envS) -> WDict(e)), capCode)

          case BuiltinFunction(bf, _) => bf match {

            case BoolEq =>
              val BoolEval(a) ~: BoolEval(b) ~: WNil() = rawArgs
              Bool(a == b, from)
            case BoolNot =>
              val BoolEval(a), BoolEval(b) ~: WNil() = rawArgs
              Bool(a != b, from)
            case DictContains =>
              val DictEval(a) ~: WEval(k) ~: WNil() = rawArgs
              Bool(a.contains(k))
            case DictGet =>
              val DictEval(d) ~: WEval(k) ~: WNil() = rawArgs
              require(d.contains(k), s"Dictionary $d did not contain $k in $fnCall")
              d(k)
            case DictInsert =>
              val DictEval(d) ~: WEval(k) ~: WEval(v) ~: WNil() = rawArgs
              WDict(d + ((k, v)))
            case DictMake => WDict(rawArgs.foldLeft(Dict) { case (p, PairEval(kv)) => p + kv }, from)
            case DictRemove =>
              val DictEval(d) ~: WEval(k) ~: WNil() = rawArgs
              require(d.contains(k), s"Dictionary $d must contain $k in order to remove it, in $fnCall")
              WDict(d - k)
            case DictSize =>
              val DictEval(d) ~: WNil() = rawArgs
              Num(d.size)
            case DictToList =>
              val DictEval(d) ~: WNil() = rawArgs
              WList(d.toSeq.map { case (k, v) => WList(Seq(k, v)) })
            case Error =>
              val err = rawArgs.map(eval(e, _)).mkString(" ")
              sys.error(s"Fatal error $err triggered by $fnCall")
            case Eval =>
              val DictEval(ue) ~: uform ~: WNil() = rawArgs
              eval(ue, eval(e, uform))
            case If =>
              val BoolEval(cond) ~: trueCase ~: falseCase ~: WNil() = rawArgs
              eval(e, if (cond) trueCase else falseCase)
            case ListCons =>
              val WEval(l) ~: WEval(e) ~: WNil() = rawArgs
              l match {
                case l: WList => WCons(e, l)
                case x => sys.error(s"Can't cons onto non-list: $l in $fnCall")
              }
            case ListHead =>
              val ListEval(l) ~: WNil() = rawArgs
              l.head
            case ListIsEmpty =>
              val ListEval(l) ~: WNil() = rawArgs
              Bool(l.isEmpty)
            case ListMake =>
              WList(rawArgs.map(eval(e, _)))
            case ListTail =>
              val WEval(l) ~: WNil() = rawArgs
              l match {
                case WCons(_, tail, _) => tail
                case WEmpty(_) => sys.error(s"Can't call tail on empty list in $fnCall")
                case x => sys.error("Can't call tail on non-list $x in $fnCall")
              }
            case NumAdd =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Num(a + b)
            case NumDiv =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              require(b != 0, s"Divisor was zero in $fn")
              Num(a / b)
            case NumEq =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Bool(a == b)
            case NumGT =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Bool(a > b)
            case NumGTE =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Bool(a >= b)
            case NumLT =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Bool(a < b)
            case NumLTE =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Bool(a <= b)
            case NumMult =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Num(a * b)
            case NumSub =>
              val NumEval(a) ~: NumEval(b) ~: WNil() = rawArgs
              Num(a - b)
            case NumToCharList =>
              val NumEval(a) ~: WNil() = rawArgs
              WList(a.toString.map(WChar(_)))
            case Parse =>
              val ListEval(letters) ~: WNil() = rawArgs
              val asString = letters.map { _.asChar.get }.mkString // ew
              WList(Parser(asString))
            case Quote =>
              val a ~: WNil() = rawArgs
              a
            case ReadFile =>
              val ListEval(fns) = rawArgs
              val fileName = fns.map { c => c.asChar.get }.mkString
              WList(io.Source.fromFile(fileName).toStream.map(WChar(_)))
            case SymEq =>
              val SymEval(a) ~: SymEval(b) ~: WNil() = rawArgs
              Bool(a == b)
            case SymToCharList =>
              val SymEval(a) ~: WNil() = rawArgs
              WList(a.name.map(WChar(_)))
            case Trace =>
              rawArgs.map(eval(e, _)).foldLeft(WEmpty(): W) {
                (p, n) =>
                  println(p)
                  n
              }
            case TypeEq =>
              val TypeEval(a) ~: TypeEval(b) ~: WNil() = rawArgs
              Bool(a == b, from)
            case TypeOf =>
              val WEval(a) ~: WNil() = rawArgs
              WType(a.typeOf, from)
            case Vau =>
              val SymEval(aS) ~: SymEval(eS) ~: WEval(code) ~: WNil() = rawArgs

              // make an exception for _ since it's so awesome
              require(aS == Symbol("_") || !e.contains(Sym(aS)), s"Found $aS in environment, in $fnCall")
              require(eS == Symbol("_") || !e.contains(Sym(eS)), s"Found $eS in environment, in $fnCall")
              require(aS == Symbol("_") || aS != eS, s"Arg symbol $aS is the same as env symbol in $fnCall")

              UDF(e, aS, eS, code, from)
          }
          case x => sys.error(s"Can not evaluate a $x in $fnCall")
        }
      case x => x // Note, this case catches an empty list too
    }

  }

}
