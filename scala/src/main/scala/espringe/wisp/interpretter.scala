package espringe.wisp

class Interpretter(dir: java.io.File) {

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
    object FnCallEval {
      def unapply(value: W) = eval(e, value).asFnCall
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
      case fnCall @ FnCall(WEval(fn), rawArgs, _) =>
        def from = new ComputedSource(fnCall)
        def evaledArgs = rawArgs.mapW(eval(e, _))

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
            case DictMake =>
              WDict(
                rawArgs.map { x =>
                  eval(e, x) match {
                    case k ~: v ~: WNil() => (k, v)
                    case x => sys.error(s"Expecting a [k v] in #dictMake, but instead got $x in $fnCall")
                  }
                }.toMap)
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
            case FnCallArgs =>
              val FnCallEval(fnc) ~: WNil() = rawArgs
              fnc.args
            case FnCallFn =>
              val FnCallEval(fnc) ~: WNil() = rawArgs
              fnc.func
            case FnCallMake =>
              rawArgs match {
                case WEval(fnc) ~: ListEval(args) ~: WNil() => FnCall(fnc, WList(args))
                case x => sys.error(s"#fn-call-make expected two arguments a function and a list, instead got: $x in $fnCall")
              }
            case If =>
              val BoolEval(cond) ~: trueCase ~: falseCase ~: WNil() = rawArgs
              eval(e, if (cond) trueCase else falseCase)
            case Let =>
              require(!rawArgs.isEmpty, "Let expect a list of bindings, and the a final statement")

              val finalEnv = rawArgs.init.foldLeft(e) { (newEnv, binding) =>
                binding match {
                  case FnCall(sym, bindee ~: WNil(), _) =>
                    require(!newEnv.contains(sym), s"In let, trying to rebind $sym in $fnCall where the env is $newEnv")
                    newEnv + ((sym, eval(newEnv, bindee)))
                  case x => sys.error(s"Let required a binding to be [Symbol Expression] but found $x in $fnCall")
                }
              }

              eval(finalEnv, rawArgs.last) // and for our tail call
            case ListCons =>
              val WEval(l) ~: WEval(e) ~: WNil() = rawArgs
              l match {
                case l: WList => new WCons(e, l)
                case x => sys.error(s"Can't cons onto non-list: $l in $fnCall")
              }
            case ListHead =>
              evaledArgs match {
                case (l ~: _) ~: WNil() => l
                case x => sys.error(s"#list-head expected a non-empty list, instead found $x in $fnCall")
              }
            case ListIsEmpty =>
              val ListEval(l) ~: WNil() = rawArgs
              Bool(l.isEmpty)
            case ListMake =>
              WList(rawArgs.map(eval(e, _)))
            case ListTail =>
              val WEval(l) ~: WNil() = rawArgs
              l match {
                case _ ~: tail => tail
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
              rawArgs match {
                case NumEval(a) ~: NumEval(b) ~: WNil() => Bool(a <= b)
                case x => sys.error(s"#num-lte expected 2 numbers, instead got: $x in $fnCall")
              }
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
              val WEval(str) ~: WNil() = rawArgs
              val fileName = str.asString.getOrElse(sys.error(s"#ReadFile expected a string (a list of chars) but got $str"))
              WList(io.Source.fromFile(new java.io.File(dir, fileName)).toStream.map(WChar(_)))
            case SymEq =>
              val SymEval(a) ~: SymEval(b) ~: WNil() = rawArgs
              Bool(a == b)
            case SymToCharList =>
              val SymEval(a) ~: WNil() = rawArgs
              WList(a.name.map(WChar(_)))
            case Then =>
              val WEval(_) ~: second ~: WNil() = rawArgs
              eval(e, second) // to be a tail call
            case Trace =>
              require(!rawArgs.isEmpty, s"Can not #trace nothing, in $fnCall")
              val results = rawArgs.map(eval(e, _))
              println(results.mkString(" "))
              results.last
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
          case x => sys.error(s"Can not evaluate $x in $fnCall")
        }
      case x => x
    }

  }

}
