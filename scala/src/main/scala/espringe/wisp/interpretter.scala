package espringe.wisp

class Interpretter(dir: java.io.File) {

  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(WDict(), form)

  def eval(e: WDict, form: W): W = {

    object X {
      def apply(value: W) = eval(e, value)
      def unapply(value: W) = Some(eval(e, value))
    }

    import BuiltinFunctionNames._

    form match {
      case s: Sym => {
        require(e.value.contains(s), s"Could not find $s in environment $e")
        e.value(s)
      }
      case fnCall @ FnCall(X(fn), rawArgs) =>
        implicit def from = new ComputedSource(fnCall)
        def evaledArgs = rawArgs.mapW(eval(e, _))

        fn match { // in order to tail call if/eval, can't just dynamic-dispatch out
          case UDF(capEnv, argS, envS, capCode) =>
            val newEnv = WDict(capEnv.value + (argS -> rawArgs) + (envS -> e))
            eval(newEnv, capCode)
          case BuiltinFunction(bf) => bf match {

            case BoolEq => evaledArgs match {
              case Bool(a) ~: Bool(b) ~: WNil =>
                Bool(a == b)
              case x => sys.error(s"Expected two booleans, instead found $x in $fnCall")
            }
            case BoolNot => evaledArgs match {
              case Bool(a) ~: Bool(b) ~: WNil =>
                Bool(a != b)
            }
            case DictContains => evaledArgs match {
              case WDict(d) ~: k ~: WNil => Bool(d.contains(k))
            }
            case DictGet => evaledArgs match {
              case WDict(d) ~: k ~: WNil =>
                require(d.contains(k), s"Dictionary $d did not contain $k in $fnCall")
                d(k)
            }
            case DictInsert => evaledArgs match {
              case WDict(d) ~: k ~: v =>
                WDict(d + ((k, v)))
            }
            case DictMake =>
              WDict(evaledArgs.map {
                _ match {
                  case k ~: v ~: WNil => (k, v)
                  case x => sys.error(s"Expecting a [k v] in #dictMake, but instead got $x in $fnCall")
                }
              }.toMap)
            case DictRemove => evaledArgs match {
              case WDict(d) ~: k ~: WNil =>
                require(d.contains(k), s"Dictionary $d must contain $k in order to remove it, in $fnCall")
                WDict(d - k)
            }
            case DictSize =>
              val X(WDict(d)) ~: WNil = rawArgs
              Num(d.size)
            case DictToList =>
              val X(WDict(d)) ~: WNil = rawArgs
              WList(d.toSeq.map { case (k, v) => WList(Seq(k, v)) })
            case Error =>
              val err = rawArgs.map(eval(e, _)).mkString(" ")
              sys.error(s"Fatal error $err triggered by $fnCall")
            case Eval =>
              rawArgs match {
                case inEnv ~: nForm ~: WNil =>
                  eval(e, inEnv) match {
                    case d: WDict => eval(d, nForm)
                    case x => sys.error(s"eval expected a Dict for first argument, instead found $x in $fnCall")
                  }
                case x => sys.error(s"eval expected two arguments, a dictionary and what to evaluate, instead found $x in $fnCall")
              }
            case FnCallArgs =>
              val X(FnCall(fnc, args)) ~: WNil = rawArgs
              args
            case FnCallFn =>
              val X(FnCall(fnc, args)) ~: WNil = rawArgs
              fnc
            case FnCallMake =>
              rawArgs match {
                case X(fnc) ~: X(args: WList) ~: WNil => FnCall(fnc, WList(args))
                case x => sys.error(s"#fn-call-make expected two arguments a function and a list, instead got: $x in $fnCall")
              }
            case If =>
              val X(Bool(c)) ~: trueCase ~: falseCase ~: WNil = rawArgs
              X(if (c) trueCase else falseCase)
            case Let =>
              require(!rawArgs.isEmpty, "Let expect a list of bindings, and the a final statement")

              val bindings = rawArgs.init.map(_ match {
                case FnCall(sym: Sym, bindee ~: WNil) => (sym, bindee)
                case x => sys.error(s"Expected (sym value) but found $x in $fnCall")
              })

              val (lb, normalBindings) = bindings.partition(_ => true)

              val lazyBindings = lb.map { case (s, v) => s -> new Lazy(v) }

              val newEnv = WDict(e.value ++ lazyBindings ++ normalBindings)

              lazyBindings.foreach { case (_, fc) => fc.setEvaler(eval(newEnv, _)) }

              eval(newEnv, rawArgs.last)
            case ListCons =>
              val X(l) ~: X(e) ~: WNil = rawArgs
              l match {
                case l: WList => WCons(e, l)
                case x => sys.error(s"Can't cons onto non-list: $l in $fnCall")
              }
            case ListHead =>
              evaledArgs match {
                case (l ~: _) ~: WNil => l
                case x => sys.error(s"#list-head expected a non-empty list, instead found $x in $fnCall")
              }
            case ListIsEmpty =>
              val X(l: WList) ~: WNil = rawArgs
              Bool(l.isEmpty)
            case ListMake =>
              WList(rawArgs.map(eval(e, _)))
            case ListTail =>
              val X(l) ~: WNil = rawArgs
              l match {
                case _ ~: tail => tail
                case WNil => sys.error(s"Can't call tail on empty list in $fnCall")
                case x => sys.error("Can't call tail on non-list $x in $fnCall")
              }
            case NumAdd =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              Num(a + b)
            case NumDiv =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              require(b != 0, s"Divisor was zero in $fn")
              Num(a / b)
            case NumEq =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              Bool(a == b)
            case NumGT =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              Bool(a > b)
            case NumGTE =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              Bool(a >= b)
            case NumLT =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              Bool(a < b)
            case NumLTE =>
              rawArgs match {
                case X(Num(a)) ~: X(Num(b)) ~: WNil => Bool(a <= b)
                case x => sys.error(s"#num-lte expected 2 numbers, instead got: $x in $fnCall")
              }
            case NumMult => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil =>
                Num(a * b)
            }
            case NumSub =>
              val X(Num(a)) ~: X(Num(b)) ~: WNil = rawArgs
              Num(a - b)
            case NumToCharList =>
              val X(Num(a)) ~: WNil = rawArgs
              WList(a.toString.map(WChar(_)))
            case Parse =>
              val X(letters: WList) ~: WNil = rawArgs
              val asString = letters.asString.getOrElse(sys.error(s"Could not treat $letters as a string for parse in $fnCall"))
              WList(Parser(asString))
            case Quote =>
              val a ~: WNil = rawArgs
              a
            case ReadFile =>
              val X(str: WList) ~: WNil = rawArgs
              val fileName = str.asString.getOrElse(sys.error(s"ReadFile expected a string (a list of chars) but got $str in $fnCall"))
              WList(io.Source.fromFile(new java.io.File(dir, fileName)).toStream.map(WChar(_)))
            case SymEq =>
              val X(Sym(a)) ~: X(Sym(b)) ~: WNil = rawArgs
              Bool(a == b)
            case SymToCharList =>
              val X(Sym(a)) ~: WNil = rawArgs
              WList(a.name.map(WChar(_)))
            case Then =>
              val X(_) ~: second ~: WNil = rawArgs
              eval(e, second) // to be a tail call
            case Trace =>
              require(!rawArgs.isEmpty, s"Can not #trace nothing, in $fnCall")
              val results = rawArgs.map(eval(e, _))
              println(results.map(_.deparse).mkString(" "))
              results.last
            case TypeEq =>
              val X(a) ~: X(b) ~: WNil = rawArgs
              Bool(a.typeOf == b.typeOf)
            case TypeOf =>
              val X(a) ~: WNil = rawArgs
              WType(a.typeOf)
            case Vau =>
              rawArgs match {
                case (aS: Sym) ~: (eS: Sym) ~: code ~: WNil =>
                  // make an exception for _ since it's so awesome
                  require(aS == Symbol("_") || !e.value.contains(aS), s"Shaddowing error. Found argS $aS in environment, in $fnCall")
                  require(eS == Symbol("_") || !e.value.contains(eS), s"Shaddowing error. Found envS $eS in environment, in $fnCall")
                  require(aS == Symbol("_") || aS != eS, s"Arg symbol $aS is the same as env symbol in $fnCall")
                  UDF(e, aS, eS, code)
                case x => sys.error(s"Unexpected arguments in vau. Expected two symbols then code (argS envS code) in $fnCall")
              }

          }
          case x => sys.error(s"Can not evaluate $x in $fnCall")
        }
      case x => x
    }

  }

}
