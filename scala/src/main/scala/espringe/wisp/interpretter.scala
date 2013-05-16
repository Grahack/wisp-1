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
    object StreamEval {
      def unapply(value: W) = eval(e, value).asStream
    }
    object PairEval {
      def unapply(value: W) = eval(e, value).asList.collect { case Stream(a, b) => (a, b) }
    }

    import BuiltinFunctionNames._

    form match {

      case fnCall @ WList(WEval(fn) #:: rawArgs, _) =>

        def from = new ComputedSource(fnCall)

        fn match { // in order to tail call if/eval, can't just dynamic-dispatch out

          case UDF(capEnv, argS, envS, capCode, _) =>
            require(rawArgs.isEmpty)
            eval(capEnv + (argS -> WList(rawArgs)) + (envS -> WDict(e)), capCode)

          case BuiltinFunction(bf, _) => bf match {

            case BoolEq =>
              val Stream(BoolEval(a), BoolEval(b)) = rawArgs
              Bool(a == b, from)
            case BoolNot =>
              val Stream(BoolEval(a), BoolEval(b)) = rawArgs
              Bool(a != b, from)
            case DictContains => {
              val Stream(DictEval(a), WEval(k)) = rawArgs
              Bool(a.contains(k))
            }
            case DictGet => {
              val Stream(DictEval(d), WEval(k)) = rawArgs

              require(d.contains(k), s"Dictionary $d did not contain $k in $fnCall")

              d(k)
            }
            case DictInsert => {
              val Stream(DictEval(d), WEval(k), WEval(v)) = rawArgs
              WDict(d + ((k, v)))
            }
            case DictRemove => {
              val Stream(DictEval(d), WEval(k)) = rawArgs
              require(d.contains(k), s"Dictionary $d must contain $k in order to remove it, in $fnCall")
              WDict(d - k)
            }
            case DictToList => {
              val Stream(DictEval(d)) = rawArgs
              WList(d.toStream.map { case (k, v) => WList(Stream(k, v)) })
            }
            case DictSize => {
              val Stream(DictEval(d)) = rawArgs
              Num(d.size)
            }
            case DictMake => WDict(rawArgs.foldLeft(Dict) { case (p, PairEval(kv)) => p + kv }, from)

            case Eval => {
              val Stream(DictEval(ue), uform) = rawArgs
              eval(ue, eval(e, uform))
            }

            case ListMake => WList(rawArgs.map(eval(e, _)), from)

            case Parse => {
              val Stream(StreamEval(letters)) = rawArgs
              val asString = letters.map { _.asChar.get }.mkString // ew
              WList(Parser(asString).toStream)
            }

            case Deref => {
              val Stream(SymEval(s)) = rawArgs
              e(s)
            }
            case If =>
              val Stream(BoolEval(cond), trueCase, falseCase) = rawArgs
              eval(e, if (cond) trueCase else falseCase)

            case ReadFile => {
              val Stream(StreamEval(fns)) = rawArgs
              val fileName = fns.map { c => c.asChar.get }.mkString
              WList(io.Source.fromFile(fileName).toStream.map(WChar(_)), from)
            }
            case Trace => {
              rawArgs.map(eval(e, _)).foldLeft(WList(Stream()): W) {
                (p, n) =>
                  println(p)
                  n
              }
            }

            case TypeEq => {
              val Stream(TypeEval(a), TypeEval(b)) = rawArgs
              Bool(a == b, from)
            }
            case TypeOf => {
              val Stream(WEval(a)) = rawArgs
              WType(a.typeOf, from)
            }
            case Vau => {
              val Stream(SymEval(aS), SymEval(eS), WEval(code)) = rawArgs

              // make an exception for _ since it's so awesome
              require(aS == Symbol("_") || !e.contains(aS), s"Found $aS in environment, in $fnCall")
              require(eS == Symbol("_") || !e.contains(eS), s"Found $eS in environment, in $fnCall")
              require(aS == Symbol("_") || aS != eS, s"Arg symbol $aS is the same as env symbol in $fnCall")

              UDF(e, aS, eS, code, from)
            }
            case ListCons => {
              val Stream(ListEval(l), WEval(e)) = rawArgs
              WList(e #:: l)
            }
            case ListHead => {
              val Stream(ListEval(l)) = rawArgs
              l.head
            }
            case ListIsEmpty => {
              val Stream(ListEval(l)) = rawArgs
              Bool(l.isEmpty)
            }
            case ListTail => {
              val Stream(ListEval(l)) = rawArgs
              WList(l.tail)
            }
            case NumAdd => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Num(a.value + b.value)
            }
            case NumDiv => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              require(b.value != 0, s"Divisor was zero in $fn")
              Num(a.value / b.value)
            }
            case NumEq => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a.value == b.value)
            }
            case NumGT => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a.value > b.value)
            }
            case NumGTE => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a.value >= b.value)
            }
            case NumLT => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a.value < b.value)
            }
            case NumLTE => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Bool(a.value <= b.value)
            }
            case NumMult => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Num(a.value * b.value)
            }
            case NumSub => {
              val Stream(NumEval(a), NumEval(b)) = rawArgs
              Num(a.value - b.value)
            }
            case NumToCharList => {
              val Stream(NumEval(a)) = rawArgs
              WList(a.toString.toStream.map(WChar(_)))
            }

            case SymEq => {
              val Stream(SymEval(a), SymEval(b)) = rawArgs
              Bool(a.value == b.value)
            }
            case SymToCharList => {
              val Stream(SymEval(a)) = rawArgs
              WList(a.value.name.toStream.map(WChar(_)))
            }

            case Error =>
              {
                sys.error(s"Fatal error, triggered by $fnCall evaled args: " + rawArgs.map(eval(e, _)).mkString(" "))
              }
              ???

          }

          case x: WChar => sys.error(s"Cannot evaluate a Char. $x in $fnCall")
          case x: WDict => sys.error(s"Cannot evalute a Dict. $x in $fnCall")
          case x: WList => sys.error(s"Cannot evalute a List? $x in $fnCall")
          case x: Sym => sys.error(s"Cannot evaluate a Symbol? $x in $fnCall")
          case x: WType => sys.error(s"Cannot evalute a Type. $x in $fnCall")
          case x: Bool => sys.error(s"Cannot evalute a Boolean. $x in $fnCall")
          case x: Num => sys.error(s"Cannot evaluate a Num. $x in $fnCall")

        }
      case x => x // Note, this case catches an empty list too
    }

  }

}
