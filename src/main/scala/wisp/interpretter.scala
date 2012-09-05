package wisp

import java.nio.file.Path
import java.nio.file.Paths
import scala.PartialFunction

object Interpretter {

  def apply(path: Path): (Any, Dict) = {

    val forms = Reader(path)

    val (imports, rest) = forms.span(_ match {
      case 'import +: _ => true
      case _ => false
    })

    val env = if (imports.isEmpty) startingEnv else {

      imports.foldLeft(Dict())((e, nextImport) => nextImport match {
        case 'import +: (importFilePath: String) +: Vect() => {
          // TODO: avoid double-loading a file
          e merge Interpretter(path.resolveSibling(importFilePath))._2
        }
        case _ => sys.error("Could not understand import")
      })

    }

    DoBlock.run(env, rest)
  }

  private def startingEnv = Dict() +
    ('true -> true) +
    ('false -> false) +
    // Some pretty primitive stuff
    ('do -> DoBlock) +
    ('eval -> Eval) +
    ('if -> If) +
    ('vau -> Vau) +
    (Symbol("'") -> Quote) +
    // some maths stuff
    (Symbol("+") -> Add) +
    (Symbol("-") -> Sub) +
    (Symbol("<") -> LessThan) +
    (Symbol("<=") -> LessThanOrEqual) +
    (Symbol("==") -> Equal) +
    (Symbol(">") -> GreaterThan) +
    (Symbol(">=") -> GreaterThanOrEqual) +
    // utility like
    ('str -> Str) +
    ('assert -> Assert) +
    ('map -> MapFunc) + // TODO: in library?
    // vect functions
    ('nth -> Nth) +
    ('length -> Length) +
    ('cons -> Cons) +
    ('list -> VecFunc) + // <-- TODO: RENAME
    (Symbol("fold-left") -> FoldLeft) +
    // Dict functions
    (Symbol("dict-insert") -> DictInsert) +
    (Symbol("dict-size") -> DictSize) +
    (Symbol("dict-contains") -> DictContains) +
    (Symbol("dict-get") -> DictGet) +
    (Symbol("dict-remove") -> DictRemove) +
    // boolean
    ('not -> Not) +
    ('and -> And) +
    ('or -> Or) +
    // debug
    ('trace -> Trace) +
    ('fails -> Fails)

  trait WVal {
    def apply(e: Dict): Any
  }

  trait WFunc {
    def apply(e: Dict, args: Vect): Any
  }

  def eval(e: Dict, form: Any): Any = {
    form match {
      case s: Symbol => {
        e(s) match {
          case v: WVal => v.apply(e)
          case x => x
        }
      }
      case head +: args => {
        val h = eval(e, head)
        require(h.isInstanceOf[WFunc], "Trying to evaluate non-function: " + h)
        h.asInstanceOf[WFunc](e, args)
      }
      case x => x
    }
  }

  object Eval extends WFunc {
    def apply(e: Dict, args: Vect) = {
      require(args.length == 2)

      eval(
        eval(e, args(0)).asInstanceOf[Dict],
        eval(e, args(1)))
    }
  }

  trait StrictFunc extends WFunc {
    def apply(e: Dict, args: Vect) = {

      val evaldArgs = args.map((eval(e, _)))

      run(evaldArgs) // TODO: nice error if it doesn't match
    }

    def run: PartialFunction[Vect, Any]
  }

  object Add extends StrictFunc {
    def run = {
      case args => args.reduce(_.asInstanceOf[Int] + _.asInstanceOf[Int])
    }
  }

  object Sub extends StrictFunc {
    def run = {
      case Vect() => 0
      case (v: Int) +: Vect() => -v
      case (head: Int) +: rest =>
        head - rest.reduce(_.asInstanceOf[Int] + _.asInstanceOf[Int]).asInstanceOf[Int]
    }
  }

  object If extends WFunc {
    def apply(e: Dict, args: Vect) = {
      require(args.length == 3)
      if (eval(e, args(0)).asInstanceOf[Boolean]) eval(e, args(1)) else eval(e, args(2))
    }
  }

  object Quote extends WFunc {
    def apply(e: Dict, args: Vect) = {
      require(args.length == 1)
      args.head // note: not eval'ing it
    }
  }

  object Vau extends WFunc {
    def apply(e: Dict, args: Vect) = {

      args match {
        case Vect(eS: Symbol, argsS: Symbol, code) => {
          require(!e.contains(eS))
          require(!e.contains(argsS))
          require(eS != argsS)
          new WFunc {
            def apply(de: Dict, args: Vect) = {
              val newEnv = e + (eS -> de) + (argsS -> args)
              eval(newEnv, code)
            }
          }
        }
        case a => sys.error("Unknown args for a lambda expression: " + a)
      }
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

  object LessThan extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = foldReduce(_ < _, e, args)
  }

  object LessThanOrEqual extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = foldReduce(_ <= _, e, args)
  }

  object Equal extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = foldReduce(_ == _, e, args)
  }

  object GreaterThan extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = foldReduce(_ > _, e, args)
  }

  object GreaterThanOrEqual extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = foldReduce(_ >= _, e, args)
  }

  object DoBlock extends WFunc {

    def run(e: Dict, forms: Vect): (Any, Dict) = {
      val (lets, rest) = forms.partition {
        _ match {
          case 'let +: (s: Symbol) +: v +: Vect() => true
          case 'let +: _ => sys.error("Malformed let")
          case _ => false
        }
      }

      object LetResult { def apply(v: Any) = new LetResult(v, false) }

      class LetResult(var payload: Any, var hasBeenEval: Boolean) extends WVal {

        def apply(u: Dict): Any = {

          if (!hasBeenEval) {
            val (capEnv, original) = payload.asInstanceOf[(Dict, Any)]
            payload = eval(capEnv, original)
            hasBeenEval = true
          }
          payload
        }
        def setEnv(e: Dict) = {
          payload = (e -> payload)
        }
      }

      val allLets = lets.data.map(x => { val y = x.asInstanceOf[Vect]; (y(1).asInstanceOf[Symbol] -> LetResult(y(2))) })

      // Now, let's add them all to a new environment

      val newEnv = allLets.foldLeft(e) {
        (oldEnv, form) =>
          require(!oldEnv.contains(form._1), "Can't redefine a symbol: " + form._1)
          oldEnv + form
      }

      // Now all our let's need a reference to the env they were defined in

      allLets.foreach { l => l._2.setEnv(newEnv) }

      // now that we done all our static environment stuff, we can go through and evaluate it all

      (rest.foldLeft(Vect(): Any)((a, b) => eval(newEnv, b)), newEnv)
    }

    def apply(e: Dict, forms: Vect): Any = {
      run(e, forms)._1
    }
  }

  object Str extends StrictFunc {
    def run = {
      case args =>
        val sb = new StringBuilder()
        args.foreach(sb.append(_))
        sb.result()
    }

  }

  object Nth extends StrictFunc {
    def run = {
      case (value: Vect) +: (index: Int) +: Vect() => value(index)
    }
  }

  object Cons extends StrictFunc {
    def run = {
      case h +: (tail: Vect) +: Vect() => Vect(h, tail)
    }
  }

  object VecFunc extends StrictFunc {
    def run = {
      case args => args
    }
  }

  object Trace extends StrictFunc {
    def run = {
      case args => println(args.mkString)
    }
  }

  object Fails extends WFunc {

    def apply(env: Dict, args: Vect) = {
      require(args.length == 1)

      try {
        eval(env, args.head)
        false
      } catch {

        case _ => true
      }
    }
  }

  object Assert extends StrictFunc {
    def run = {
      case (res: Boolean) +: Vect() => if (!res) sys.error("Code assertion failed!")
      case (res: Boolean) +: (msg: String) +: Vect() => if (!res) sys.error("Code assertion failed, with errror: " + msg)
    }
  }

  object Length extends StrictFunc {
    def run = {
      case (l: Vect) +: Vect() => l.length
    }
  }

  object DictSize extends StrictFunc {
    def run = {
      case (lu: Dict) +: Vect() => lu.size
    }
  }

  object DictGet extends StrictFunc {
    def run = {
      case (lu: Dict) +: k +: Vect() => lu(k)
    }
  }

  // TODO: this should be able to be a StricTfunc, but we need access to 'e' to call 'f'
  object MapFunc extends WFunc {
    def apply(e: Dict, args: Vect): Any = {
      require(args.length == 2)
      val f = eval(e, args(0)).asInstanceOf[WFunc]
      val l = eval(e, args(1)).asInstanceOf[Vect]

      l.map(x => f(e, Vect(x)))
    }
  }

  object FoldLeft extends WFunc {
    def apply(e: Dict, args: Vect): Any = {
      require(args.length == 3)
      val vec = eval(e, args(0)).asInstanceOf[Vect]
      val start = args(1)
      val func = eval(e, args(2)).asInstanceOf[WFunc]

      val res = vec.foldLeft(start)((a, b) => Vect(Quote, func(e, Vect(a, b))))
      // and now we need to remove the quote part from the return
      eval(e, res)
    }
  }

  object DictInsert extends StrictFunc {
    def run = {
      case (lu: Dict) +: k +: v +: Vect() => {
        lu + (k -> v)
      }
    }
  }

  object DictRemove extends StrictFunc {
    def run = {
      case (lu: Dict) +: k +: Vect() => lu - k
    }
  }

  object DictContains extends StrictFunc {
    def run = {
      case (lu: Dict) +: k +: Vect() => lu.contains(k)
    }
  }

  object Not extends StrictFunc {
    def run = {
      case (arg: Boolean) +: Vect() => !arg
    }
  }

  object And extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = {

      require(args.length >= 2)

      args.foreach { v =>
        if (eval(e, v).asInstanceOf[Boolean] == false)
          return false
      }

      true

    }
  }

  object Or extends WFunc {
    def apply(e: Dict, args: Vect): Boolean = {
      require(args.length >= 2)

      args.foreach { v =>
        if (eval(e, v).asInstanceOf[Boolean] == true)
          return true
      }

      false
    }
  }

}
