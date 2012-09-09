package wisp

import com.sun.org.apache.xpath.internal.objects.GreaterThanOrEqualComparator
import scala.xml.TypeSymbol

object Interpretter {

  import java.nio.file.Path
  import java.nio.file.Paths

  def apply(path: Path): (Any, Dict, IndexedSeq[Path]) = {

    val forms = Reader(path)

    val (imports, rest) = forms.span(_ match {
      case 'import +: _ => true
      case _ => false
    })

    val (env, paths) =
      imports.foldLeft((startingEnv, IndexedSeq(path)))(
        (ps, nextImport) =>
          nextImport match {
            case 'import +: (importFilePath: String) +: Vect() => {
              // TODO: avoid double-loading a file

              val (_, pe, pp) = Interpretter(path.resolveSibling(importFilePath))

              ((ps._1 merge pe) -> (ps._2 ++ pp))
            }
            case _ => sys.error("Could not understand import")
          })

    val (statements, newEnv) = buildDoBlock(env, rest)

    val res = statements.foldLeft(Vect(): Any)((a, b) => eval(newEnv, b))

    (res, newEnv, paths)
  }

  // object FuncNumber extends

  object WTypes extends Enumeration {
    type WType = Value
    val TypeBool, TypeSym, TypeNum, TypeDict, TypeStr, TypeVect, TypeType = Value
  }

  import WTypes._

  def eval(e: Dict, form: Any): Any = {
    form match {
      case lr: LetResult => lr()
      case s: Symbol => {
        require(s != Symbol("_"), "You really shouldn't use _ as a resolvable symbol")
        e(s) match {
          case lr: LetResult => lr()
          case x => x
        }
      }

      case f +: rawArgs => {

        def evaledArgs() = rawArgs.map(eval(e, _))

        val func = eval(e, f)

        func.asInstanceOf[WFunc] match {
          case VauRun(capEnv, envS, argS, capCode) => eval(capEnv + (envS -> e) + (argS -> rawArgs), capCode)

          // primitive stuff
          case Do => {
            val (statements, newEnv) = buildDoBlock(e, rawArgs)
            require(statements.nonEmpty)
            statements.init.foreach {
              eval(newEnv, _)
            }
            // and for a nice tail call
            eval(newEnv, statements.last)
          }
          case Eval => evaledArgs() match {
            case Vect(env: Dict, v) => eval(env, v)
          }
          case If => rawArgs match {
            case Vect(cond, trueCase, falseCase) => if (eval(e, cond).asInstanceOf[Boolean]) eval(e, trueCase) else eval(e, falseCase)
          }
          case Quote => rawArgs match {
            case Vect(x) => x
          }
          case Vau => rawArgs match {
            case Vect(envS: Symbol, argS: Symbol, code) =>
              require(!e.contains(envS), "Can't use symbol " + envS + " for binding an environment, as it already exists")
              require(!e.contains(argS), "Can't use symbol " + argS + " for binding an argument list, as it already exists")
              require(envS != argS, "Can't use the same symbol for binding the environment and argument")
              VauRun(e, envS, argS, code)
          }

          // type stuff
          case TypeEq => evaledArgs() match {
            case Vect(a: WType, b: WType) => a == b
          }

          case TypeOf => evaledArgs() match {
            case Vect(a) => a match {
              case _: Boolean => TypeBool
              case _: Int => TypeNum
              case _: String => TypeStr
              case _: Symbol => TypeSym
              case _: Vect => TypeVect
              case _: Dict => TypeDict
              case _: WType => TypeType
            }
          }

          // number stuff
          case NumAdd => evaledArgs() match {
            case Vect(a: Int, b: Int) => a + b
          }
          case NumDiv => evaledArgs() match {
            case Vect(a: Int, b: Int) => a / b
          }
          case NumEq => evaledArgs() match {
            case Vect(a: Int, b: Int) => a == b
          }
          case NumNeq => evaledArgs() match {
            case Vect(a: Int, b: Int) => a != b
          }
          case NumGreaterThan => evaledArgs() match {
            case Vect(a: Int, b: Int) => a > b
          }
          case NumGreaterThanOrEqual => evaledArgs() match {
            case Vect(a: Int, b: Int) => a >= b
          }
          case NumLessThan => evaledArgs() match {
            case Vect(a: Int, b: Int) => a < b
          }
          case NumLessThanOrEqual => evaledArgs() match {
            case Vect(a: Int, b: Int) => a <= b
          }
          case NumMult => evaledArgs() match {
            case Vect(a: Int, b: Int) => a * b
          }
          case NumSub => evaledArgs() match {
            case Vect(a: Int, b: Int) => a - b
          }

          case NumToString => evaledArgs() match {
            case Vect(a: Int) => a.toString()
          }

          // string stuff
          case StrCharAt => evaledArgs() match {
            case Vect(str: String, at: Int) => str.charAt(at).toString
          }
          case StrConcat => evaledArgs() match {
            case Vect(a: String, b: String) => a + b
          }
          case StrEq => evaledArgs() match {
            case Vect(a: String, b: String) => a == b
          }
          case StrIndexOf => evaledArgs() match {
            case Vect(str: String, search: String, startIndex: Int) => str.indexOf(search, startIndex)
          }
          case StrLastIndexOf => evaledArgs() match {
            case Vect(str: String, search: String, lastIndex: Int) => str.lastIndexOf(search, lastIndex)
          }
          case StrLength => evaledArgs() match {
            case Vect(str: String) => str.length
          }
          case StrSlice => evaledArgs() match {
            case Vect(str: String, from: Int, until: Int) => str.slice(from, until)
          }
          case StrSplit => evaledArgs() match {
            case Vect(str: String, using: String) => Vect(str.split(using): _*) // TODO: careful, is this using regex?
          }
          case StrToVect => evaledArgs() match {
            case Vect(str: String) => Vect(str.toCharArray().map(x => x.toString): _*)
          }

          // symbol stuff
          case SymToString => evaledArgs() match {
            case Vect(sym: Symbol) => sym.name
          }
          case SymEq => evaledArgs() match {
            case Vect(a: Symbol, b: Symbol) => a == b
          }

          // vector stuff
          case VectAppend => evaledArgs() match {
            case Vect(vect: Vect, v) => vect.append(v)
            case x => sys.error("args were: " + x)
          }
          case VectCons => evaledArgs() match {
            case Vect(vect: Vect, v) => vect.cons(v)
          }
          case VectSlice => evaledArgs() match {
            case Vect(vect: Vect, from: Int, until: Int) => vect.slice(from, until)
          }

          case VectNth => evaledArgs() match {
            case Vect(vect: Vect, index: Int) => vect(index)
          }

          case VectReduce => evaledArgs() match {
            case Vect(vect: Vect, func) => {
              eval(e, vect.reduce((a, b) => Vect(Quote, eval(e, Vect(func, a, b)))))
            }
          }

          // dictionary stuff
          case DictContains => evaledArgs() match {
            case Vect(lu: Dict, k) => lu.contains(k)
          }

          case DictGet => evaledArgs() match {
            case Vect(dict: Dict, k) => dict(k)
          }

          case DictInsert => evaledArgs() match {
            case Vect(dict: Dict, k, v) => dict + (k -> v)
          }

          case DictRemove => evaledArgs() match {
            case Vect(dict: Dict, k) => dict - k
          }

          case DictSize => evaledArgs() match {
            case Vect(dict: Dict) => dict.size
          }

          case DictToVect => evaledArgs() match {
            case Vect(a: Dict) => a.data.foldLeft(Vect()) { (p, n) => p.append(n) }
          }

          // boolean stuff 

          case BoolNot => evaledArgs() match {
            case Vect(arg: Boolean) => !arg
          }
          case BoolAnd => {
            require(rawArgs.length >= 2)
            rawArgs.data.forall(eval(e, _).asInstanceOf[Boolean] == true)
          }
          case BoolOr => {
            require(rawArgs.length >= 2)
            !rawArgs.data.forall(eval(e, _).asInstanceOf[Boolean] == false)
          }
          case BoolEq => evaledArgs match {
            case Vect(a: Boolean, b: Boolean) => a == b
          }

          // debug stuff

          case Error => evaledArgs() match {
            case Vect() => sys.error("Code called an error")
            case Vect(msg: String) => sys.error("Code called an errror with msg: " + msg)
          }
          case Fails => rawArgs match {
            case Vect(arg) => try {
              eval(e, arg)
              false
            } catch {
              case _: Throwable => true
            }
          }
          case Trace => println(evaledArgs().mkString)
          case VectLength => evaledArgs() match {
            case Vect(vec: Vect) => vec.length
          }

          // performance hacks
          case Fn => rawArgs match {
            case Vect(symbols: Vect, body) =>

              val processedSymbols = for (s <- symbols.data) yield {
                !e.contains(s)
                s.asInstanceOf[Symbol]
              }

              // TODO: should check that none of the processedSymbols are the same

              FnRun(e, processedSymbols, body)
          }
          case FnRun(capEnv, symbols, body) => {
            val arguments = evaledArgs
            require(symbols.length == arguments.length, "Function expected: " + symbols.length + " arguments, but got: " + arguments + " instead")

            val newEnv = symbols.zip(arguments.data).foldLeft(capEnv) { (a, b) =>

              if (b._1 == Symbol("_"))
                a
              else
                a + b
            }
            eval(newEnv, body)
          }
        }
      }
      case x => x
    }
  }

  def foldReduce(op: (Int, Int) => Boolean, e: Dict, args: Vect): Boolean = {
    require(args.length > 1)

    var acc = eval(e, args.head).asInstanceOf[Int]

    args.tail.foreach { a =>
      val r = eval(e, a).asInstanceOf[Int]
      if (op(acc, r))
        acc = r
      else
        return false
    }

    true
  }

  def strict = false

  def buildDoBlock(e: Dict, forms: Vect): (IndexedSeq[Any], Dict) = {
    def letBuilder(form: Any, v: LetResult): Iterable[(Option[Symbol], LetResult)] = {

      form match {
        case Symbol("_") => None
        case s: Symbol => Iterable(Some(s) -> v)
        case pattern: Vect => {
          require(pattern.nonEmpty)

          val (single, multi) = pattern.span(_ != Symbol("&"))

          val built = (None -> v) +: single.data.zipWithIndex.flatMap {
            case (s, i) => letBuilder(s, LetResult(Vect(VectNth, v, i)))
          }

          if (multi.nonEmpty) {
            assert(multi(0) == Symbol("&"))
            require(multi.length == 2)

            v.setPostFunction {
              r =>
                assert(r.isInstanceOf[Vect])
                assert(r.asInstanceOf[Vect].length > single.length)
                r
            }

            val brokenResult = LetResult(v)
            brokenResult.setPostFunction {
              r =>
                val v = r.asInstanceOf[Vect]
                v.slice(single.length, v.length)
            }

            (Some(multi(1).asInstanceOf[Symbol]) -> brokenResult) +: built

          } else {
            v.setPostFunction { r => r.asInstanceOf[Vect].length == single.length; r }
            built
          }

        }
        case x => sys.error("Unknown form: " + x + " in let")
      }

    }

    val lets = forms.data.flatMap {
      _ match {
        case Vect('let, binding, v) => letBuilder(binding, LetResult(v))
        case 'let +: _ => sys.error("Malformed let statement")
        case _ => None
      }
    }

    val rest = forms.data.flatMap {
      _ match {
        case 'let +: _ => None
        case x => Some(x)
      }
    }

    // Now, let's add them all to a new environment

    val newEnv = lets.foldLeft(e) {
      (oldEnv, form) =>
        form._1.map {
          name =>
            require(!oldEnv.contains(name), "Can't redefine a symbol: " + name)
            oldEnv + (name -> form._2)
        }.getOrElse(oldEnv)
    }

    // Now all our let's need a reference to the env they were defined in

    lets.foreach { l => l._2.setEnv(newEnv) }

    // now that we done all our static environment stuff, we can go through and evaluate it all

    (rest, newEnv)
  }

  object LetResult { def apply(v: Any) = new LetResult(v, false) }

  class LetResult(var payload: Any, var hasBeenEval: Boolean, var post: Any => Any = identity _) {

    def apply(): Any = {

      if (!hasBeenEval) {
        val (capEnv, original) = payload.asInstanceOf[(Dict, Any)]
        payload = post(eval(capEnv, original))

        hasBeenEval = true
      }
      payload
    }

    def setPostFunction(c: Any => Any) = {
      post = c
    }

    def setEnv(e: Dict) = {
      payload = (e -> payload)
    }

    override def toString = "{LetResult}"
  }

  sealed abstract class WFunc

  // primitive functions
  object Do extends WFunc
  object Eval extends WFunc
  object If extends WFunc
  object Quote extends WFunc
  object Vau extends WFunc

  // type stuff
  object TypeEq extends WFunc
  object TypeOf extends WFunc

  // number stuff
  object NumAdd extends WFunc
  object NumDiv extends WFunc

  object NumGreaterThan extends WFunc
  object NumGreaterThanOrEqual extends WFunc
  object NumEq extends WFunc
  object NumNeq extends WFunc
  object NumLessThan extends WFunc
  object NumLessThanOrEqual extends WFunc
  object NumMult extends WFunc
  object NumSub extends WFunc
  object NumToString extends WFunc

  // string stuff
  object StrCharAt extends WFunc
  object StrConcat extends WFunc
  object StrEq extends WFunc
  object StrIndexOf extends WFunc
  object StrLastIndexOf extends WFunc
  object StrLength extends WFunc
  object StrSlice extends WFunc
  object StrSplit extends WFunc
  object StrToVect extends WFunc

  // sym stuff
  object SymToString extends WFunc
  object SymEq extends WFunc

  // Vector Stuff
  object VectAppend extends WFunc
  object VectCons extends WFunc
  object VectLength extends WFunc
  object VectNth extends WFunc
  object VectReduce extends WFunc // <-- this is important, as it will be used for compiler optimizations
  object VectSlice extends WFunc

  // dict stuff
  object DictContains extends WFunc
  object DictGet extends WFunc
  object DictInsert extends WFunc
  object DictRemove extends WFunc
  object DictSize extends WFunc
  object DictToVect extends WFunc

  // boolean stuff
  object BoolNot extends WFunc
  object BoolAnd extends WFunc
  object BoolOr extends WFunc
  object BoolEq extends WFunc

  // debug stuff
  object Trace extends WFunc
  object Fails extends WFunc
  object Error extends WFunc

  // performance hacks
  object Fn extends WFunc

  case class VauRun(capEnv: Dict, envS: Symbol, argS: Symbol, capCode: Any) extends WFunc {
    override def toString = "$vau$"
  }

  // bit of a hack
  case class FnRun(capEnv: Dict, symbols: Seq[Symbol], capCode: Any) extends WFunc {
    override def toString = "$fnrun"
  }

  private def startingEnv = Dict() +
    // Some pretty primitive stuff
    (Symbol("#do") -> Do) +
    (Symbol("#eval") -> Eval) +
    (Symbol("#if") -> If) +
    (Symbol("#quote") -> Quote) +
    (Symbol("#vau") -> Vau) +
    // Types
    (Symbol("#Num") -> TypeNum) +
    (Symbol("#Str") -> TypeStr) +
    (Symbol("#Bool") -> TypeBool) +
    (Symbol("#Vect") -> TypeVect) +
    (Symbol("#Sym") -> TypeSym) +
    (Symbol("#Dict") -> TypeDict) +
    (Symbol("#Type") -> TypeType) +
    (Symbol("#type-eq") -> TypeEq) +
    (Symbol("#type-of") -> TypeOf) +
    // some num stuff
    (Symbol("#num-add") -> NumAdd) +
    (Symbol("#num-div") -> NumDiv) +
    (Symbol("#num-eq") -> NumEq) +
    (Symbol("#num-gt") -> NumGreaterThan) +
    (Symbol("#num-gte") -> NumGreaterThanOrEqual) +
    (Symbol("#num-lt") -> NumLessThan) +
    (Symbol("#num-lte") -> NumLessThanOrEqual) +
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
    (Symbol("#bool-and") -> BoolAnd) +
    (Symbol("#bool-eq") -> BoolEq) +
    (Symbol("#bool-false") -> false) +
    (Symbol("#bool-not") -> BoolNot) +
    (Symbol("#bool-or") -> BoolOr) +
    (Symbol("#bool-true") -> true) +
    // debug
    (Symbol("#error") -> Error) +
    (Symbol("#fails") -> Fails) +
    (Symbol("#trace") -> Trace) +
    // performance hacks
    (Symbol("#fn") -> Fn)
}
