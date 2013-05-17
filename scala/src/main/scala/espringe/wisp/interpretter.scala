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
      def unapply(value: W) = eval(e, value).asList.map { case Stream(a, b) => (a, b) }
    }

    import BuiltinFunctionNames._

    form match {
      case s: Sym => {
        require(e.contains(s), s"Could not find $s in environment $e")
        e(s)
      }
      case fnCall @ WList(WEval(fn) #:: rawArgs, _) =>
        def from = new ComputedSource(fnCall)
        fn match { // in order to tail call if/eval, can't just dynamic-dispatch out
          case UDF(capEnv, argS, envS, capCode, _) =>
            require(rawArgs.isEmpty)
            eval(capEnv + (Sym(argS) -> WList(rawArgs)) + (Sym(envS) -> WDict(e)), capCode)

          case BuiltinFunction(bf, _) => bf match {

            case BoolEq =>
              val Stream(BoolEval(a), BoolEval(b)) = rawArgs
              Bool(a == b, from)
            case BoolNot =>
              val Stream(BoolEval(a), BoolEval(b)) = rawArgs
              Bool(a != b, from)
            case DictContains =>
              val Stream(DictEval(a), WEval(k)) = rawArgs
              Bool(a.contains(k))
            case DictGet =>
              val Stream(DictEval(d), WEval(k)) = rawArgs
              require(d.contains(k), s"Dictionary $d did not contain $k in $fnCall")
              d(k)
            case DictInsert =>
              val Stream(DictEval(d), WEval(k), WEval(v)) = rawArgs
              WDict(d + ((k, v)))
            case DictMake => WDict(rawArgs.foldLeft(Dict) { case (p, PairEval(kv)) => p + kv }, from)
            case DictRemove =>
              val Stream(DictEval(d), WEval(k)) = rawArgs
              require(d.contains(k), s"Dictionary $d must contain $k in order to remove it, in $fnCall")
              WDict(d - k)
            case DictSize =>
              val Stream(DictEval(d)) = rawArgs
              Num(d.size)
            case DictToList =>
              val Stream(DictEval(d)) = rawArgs
              WList(d.toStream.map { case (k, v) => WList(Stream(k, v)) })
            case Error =>
              val err = rawArgs.map(eval(e, _)).mkString(" ")
              sys.error(s"Fatal error $err triggered by $fnCall")
            case Eval =>
              val Stream(DictEval(ue), uform) = rawArgs
              eval(ue, eval(e, uform))
            case If =>
              val Stream(BoolEval(cond), trueCase, falseCase) = rawArgs
              eval(e, if (cond) trueCase else falseCase)
            case ListCons =>
              val Stream(ListEval(l), WEval(e)) = rawArgs
              WList(e #:: l)
            case ListHead =>
              val Stream(ListEval(l)) = rawArgs
              l.head
            case ListIsEmpty =>
              val Stream(ListEval(l)) = rawArgs
              Bool(l.isEmpty)
            case ListMake =>
              WList(rawArgs.map(eval(e, _)), from)
            case ListTail =>
              val Stream(ListEval(l)) = rawArgs
              WList(l.tail)
            case NumAdd =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Num(a + b)
            case NumDiv =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              require(b != 0, s"Divisor was zero in $fn")
              Num(a / b)
            case NumEq =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a == b)
            case NumGT =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a > b)
            case NumGTE =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a >= b)
            case NumLT =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a < b)
            case NumLTE =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a <= b)
            case NumMult =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Num(a * b)
            case NumSub =>
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Num(a - b)
            case NumToCharList =>
              val Stream(NumEval(a)) = rawArgs
              WList(a.toString.toStream.map(WChar(_)))
            case Parse =>
              val Stream(ListEval(letters)) = rawArgs
              val asString = letters.map { _.asChar.get }.mkString // ew
              WList(Parser(asString).toStream)
            case Quote =>
              val Stream(a) = rawArgs
              a
            case ReadFile =>
              val Stream(ListEval(fns)) = rawArgs
              val fileName = fns.map { c => c.asChar.get }.mkString
              WList(io.Source.fromFile(fileName).toStream.map(WChar(_)), from)
            case SymEq =>
              val Stream(SymEval(a), SymEval(b)) = rawArgs
              Bool(a == b)
            case SymToCharList =>
              val Stream(SymEval(a)) = rawArgs
              WList(a.name.toStream.map(WChar(_)))
            case Trace =>
              rawArgs.map(eval(e, _)).foldLeft(WList(Stream()): W) {
                (p, n) =>
                  println(p)
                  n
              }
            case TypeEq =>
              val Stream(TypeEval(a), TypeEval(b)) = rawArgs
              Bool(a == b, from)
            case TypeOf =>
              val Stream(WEval(a)) = rawArgs
              WType(a.typeOf, from)
            case Vau =>
              val Stream(SymEval(aS), SymEval(eS), WEval(code)) = rawArgs

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
