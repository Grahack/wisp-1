package espringe.wisp

class Interpretter(dir: java.io.File) {

  import scala.collection.immutable.HashMap

  def apply(form: W): W = eval(WDict(), form)

  private def eval(e: WDict, form: W): W = try {
    import BuiltinFunction._

    form match {
      case s: Sym => {
        require(e.value.contains(s), s"Could not find $s in environment $e")
        e.value(s)
      }
      case fnCall @ FnCall(fn, rawArgs) =>
        implicit def from = new ComputedSource(fnCall)
        def evaledArgs = rawArgs.mapW(eval(e, _))

        eval(e, fn) match { // in order to tail call if/eval, can't just dynamic-dispatch out
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
            case DictSize => evaledArgs match {
              case WDict(d) ~: WNil => Num(d.size)
            }
            case DictToList => evaledArgs match {
              case WDict(d) ~: WNil => WList(d.map { case (k, v) => WList(Seq(k, v)) })
            }
            case Error =>
              val err = evaledArgs.map(_.deparse).mkString(" ")
              sys.error(s"Fatal error $err triggered by $fnCall")
            case Eval =>
              rawArgs match {
                case inEnv ~: nForm ~: WNil =>
                  eval(e, inEnv) match {
                    case d: WDict => eval(d, nForm)
                    case x => sys.error(s"eval expected a dict for first argument, instead found $x in $fnCall")
                  }
                case x => sys.error(s"eval expected two arguments, a dictionary and what to evaluate, instead found $x in $fnCall")
              }
            case FnCallArgs => evaledArgs match {
              case FnCall(_, args) => args
            }
            case FnCallFn => evaledArgs match {
              case FnCall(fn, _) => fn
            }
            case FnCallMake => evaledArgs match {
              case fn ~: (args: WList) ~: WNil => FnCall(fn, WList(args))
              case x => sys.error(s"#fn-call-make expected two arguments a function and a list, instead got: $x in $fnCall")
            }
            case If => rawArgs match {
              case cond ~: trueCase ~: falseCase ~: WNil =>
                eval(e, cond) match {
                  case Bool(b) => eval(e, if (b) trueCase else falseCase)
                  case x => sys.error(s"The condition in if must be a boolean, found $x in $fnCall")
                }
              case x => sys.error(s"if expects [cond true false] but found $x in $fnCall")
            }
            case Let =>
              require(!rawArgs.isEmpty, "Let expect a list of bindings, and the a final statement")

              val bindings = rawArgs.init.map(_ match {
                case FnCall(sym: Sym, bindee ~: WNil) => (sym, bindee)
                case x => sys.error(s"Expected (sym value) but found $x in $fnCall")
              })

              val (lb, normalBindings) = bindings.partition { case (_, b) => b.isInstanceOf[FnCall] || b.isInstanceOf[Sym] }

              val lazyBindings = lb.map { case (s, v) => (s, new Lazy(v)) }

              val newEnv = WDict(e.value ++ lazyBindings ++ normalBindings)

              lazyBindings.foreach { case (_, fc) => fc.setEvaler(eval(newEnv, _)) }

              // A big optimization here would be to resolve all symbols (when possible)

              eval(newEnv, rawArgs.last)
            case ListCons => evaledArgs match {
              case (l: WList) ~: e ~: WNil => WCons(e, l)
              case x => sys.error(s"Can't cons, expect [list value] got $x in $fnCall")
            }
            case ListHead => evaledArgs match {
              case (l ~: _) ~: WNil => l
              case x => sys.error(s"list-head expected a non-empty list, instead found $x in $fnCall")
            }
            case ListIsEmpty => evaledArgs match {
              case (l: WList) ~: WNil => Bool(l.isEmpty)
            }
            case ListMake => WList(evaledArgs)
            case ListTail => evaledArgs match {
              case (_ ~: tail) ~: WNil => tail
              case x => sys.error(s"Expected a non-empty list, but found $x in $fnCall")
            }
            case NumAdd => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Num(a + b)
              case x => sys.error(s"num-add expected [Num Num] but found $x in $fnCall")
            }
            case NumDiv => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil if b != 0 => Num(a / b)
              case x => sys.error(s"num-div expected [Num Num] (with nonzero divisor), found: $x in $fnCall")
            }
            case NumEq => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Bool(a == b)
              case x => sys.error(s"num-eq expected [Num Num] but found $x in $fnCall")
            }
            case NumGT => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Bool(a > b)
              case x => sys.error(s"num-gt expected [Num Num] but found $x in $fnCall")
            }
            case NumGTE => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Bool(a >= b)
              case x => sys.error(s"num-gte expected [Num Num] but found $x in $fnCall")
            }
            case NumLT => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Bool(a < b)
              case x => sys.error(s"num-lt expected [Num Num] but found $x in $fnCall")
            }
            case NumLTE => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Bool(a <= b)
              case x => sys.error(s"num-lte expected [Num Num] but found $x in $fnCall")
            }
            case NumMult => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Num(a * b)
              case x => sys.error("num-mult expected [Num Num] but found $x in $fnCall")
            }
            case NumSub => evaledArgs match {
              case Num(a) ~: Num(b) ~: WNil => Num(a - b)
              case x => sys.error(s"num-sub expected [Num Num] but found $x in $fnCall")
            }
            case NumToCharList => evaledArgs match {
              case Num(a) ~: WNil => WList(a.toString.map(WChar(_)))
              case x => sys.error(s"num-to-char-list expected [Num] but found $x in $fnCall")
            }
            case Parse => evaledArgs match {
              case (letters: WList) ~: WNil =>
                val asString = letters.asString.getOrElse(sys.error(s"Could not treat $letters as a string for parse in $fnCall"))
                WList(Parser(asString))
              case x => sys.error(s"parse expected a list of characters, instead found $x in $fnCall")
            }
            case Quote => rawArgs match {
              case a ~: WNil => a
              case x => sys.error(s"quote expected a single argument, found $x in $fnCall")
            }
            case ReadFile => evaledArgs match {
              case (str: WList) ~: WNil =>
                val fileName = str.asString.getOrElse(sys.error(s"ReadFile expected a string (a list of chars) but got $str in $fnCall"))
                WList(io.Source.fromFile(new java.io.File(dir, fileName)).toStream.map(WChar(_)))
            }
            case SymEq => evaledArgs match {
              case Sym(a) ~: Sym(b) ~: WNil => Bool(a == b)
              case x => sys.error("sym-eq expected [Sym Sym] but found $x in $fnCall")
            }
            case SymToCharList => evaledArgs match {
              case Sym(a) ~: WNil => WList(a.name.map(WChar(_)))
              case x => sys.error(s"sym-to-char-list expected a single symbol, found $x in $fnCall")
            }
            case Then => rawArgs match { // TODO: should this function exist?
              case a ~: b ~: WNil =>
                eval(e, a)
                eval(e, b) // to be a tail call
              case x => sys.error(s"then expected two arguments, but found $x in $fnCall")
            }
            case Trace =>
              require(!rawArgs.isEmpty, s"Can not #trace nothing, in $fnCall")
              val results = rawArgs.map(eval(e, _))
              println(results.map(_.deparse).mkString(" "))
              results.last
            case TypeEq => evaledArgs match {
              case a ~: b ~: WNil => Bool(a.typeOf == b.typeOf)
            }
            case TypeOf => evaledArgs match {
              case a ~: WNil => WType(a.typeOf)
            }
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

  } catch {
    case ex: Throwable =>
      System.err.println(ex.getStackTraceString);
      System.err.println(s"\nException $ex was thrown")
      System.err.println("\n..dumping core to wisp-core.txt")

      val pw = new java.io.PrintWriter("wisp-core.txt")
      pw.write(CoreDump(form))
      pw.close()

      System.exit(-1)
      ???
  }

}
