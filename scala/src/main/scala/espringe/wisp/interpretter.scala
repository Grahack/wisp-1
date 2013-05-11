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

    form match {

      case fnCall @ WList(WEval(fn) #:: rawArgs, _) =>
        
        def from = new ComputedSource(fnCall)

        fn match { // in order to tail call if/eval, can't just dynamic-dispatch out

          case UDF(capEnv, argS, envS, capCode, _) =>
            require(rawArgs.isEmpty)
            eval(capEnv + (argS -> WList(rawArgs)) + (envS -> WDict(e)), capCode)
          case _: If =>
            val Stream(BoolEval(cond), trueCase, falseCase) = rawArgs
            eval(e, if (cond) trueCase else falseCase)
          case _: Eval =>
            val Stream(DictEval(ue), uform) = rawArgs
            eval(ue, eval(e, uform))
            
          case _: BoolEq =>
            val Stream(BoolEval(a), BoolEval(b)) = rawArgs
            Bool(a == b, from)
          case _: BoolNot =>
            val Stream(BoolEval(a), BoolEval(b)) = rawArgs
            Bool(a != b, from)
          case _: DictMake => WDict(rawArgs.foldLeft(Dict) { case (p, PairEval(kv)) => p + kv }, from)
          case _: ListMake => WList(rawArgs.map(eval(e, _)), from)
          case _: Parse =>
            val Stream(StreamEval(letters)) = rawArgs
            val asString = letters.map { _.asChar.get }.mkString // ewwwwwwwww
            Parser(asString) // TODO: List[W] 
          case _: Deref =>
            val Stream(SymEval(s)) = rawArgs
            e(s) // TODO: add from
          case _: ReadFile => {
            val Stream(StreamEval(fns)) = rawArgs
            val fileName = fns.map { c => c.asChar.get }.mkString
            WList( io.Source.fromFile(fileName).toStream.map(WChar(_)), from)
          }
          case _: TypeEq => {
            val Stream(TypeEval(a), TypeEval(b)) = rawArgs
            Bool(a == b, from)
          }
          case _: TypeOf => {
            val Stream(WEval(a)) = rawArgs
            WType(a.typeOf, from)
          }
          case _: Vau => {
            val Stream(SymEval(aS), SymEval(eS), WEval(code)) = rawArgs

            // make an exception for _ since it's so awesome
            require(aS == Symbol("_") || !e.contains(aS), s"Found $aS in environment, in $fnCall")
            require(eS == Symbol("_") || !e.contains(eS), s"Found $eS in environment, in $fnCall")
            require(aS == Symbol("_") || aS != eS, s"Arg symbol $aS is the same as env symbol in $fnCall")

            UDF(e, aS, eS, code, from)
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
