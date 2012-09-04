package wisp

import java.nio.file.Path
import java.nio.file.Paths
import scala.PartialFunction

object Interpretter {

  def apply(path: Path): (Any, Dict) = {

    val forms = Reader(path)

    val (imports, rest) = forms.span(_ match {
      case 'import :: _ => true
      case _ => false
    })

    val env = if (imports.isEmpty) startingEnv else {

      imports.foldLeft(Dict())((e, nextImport) => nextImport match {
        case 'import :: (importFilePath: String) :: Nil => {
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
    ('nth -> Nth) +
    ('assert -> Assert) +
    ('length -> Length) +
    ('map -> MapFunc) + // TODO: in library?
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
    ('trace -> Trace)

  trait WVal {
    def apply(e: Dict): Any
  }

  trait WFunc {
    def apply(e: Dict, args: List[Any]): Any
  }

  def eval(e: Dict, form: Any): Any = {
    form match {
      case s: Symbol => {
        e(s) match {
          case v: WVal => v.apply(e)
          case x => x
        }
      }
      case head :: args => {
        val h = eval(e, head)
        require(h.isInstanceOf[WFunc])
        h.asInstanceOf[WFunc](e, args)
      }
      case x => x
    }
  }

  object Eval extends WFunc {
    def apply(e: Dict, args: List[Any]) = {
      require(args.size == 2)

      eval(
        eval(e, args(0)).asInstanceOf[Dict],
        eval(e, args(1)))
    }
  }

  trait StrictFunc extends WFunc {
    def apply(e: Dict, args: List[Any]) = {
      val evaldArgs = args.map(eval(e, _))

      run(evaldArgs) // TODO: nice error if it doesn't match
    }

    def run: PartialFunction[List[Any], Any]
  }

  object Add extends StrictFunc {
    def run = {
      case args => args.map(_.asInstanceOf[Int]).reduce(_ + _)
    }
  }

  object Sub extends StrictFunc {
    def run = {
      case Nil => 0
      case (v: Int) :: Nil => -v
      case args =>
        val values = args.map(_.asInstanceOf[Int])
        values.head - values.tail.reduce(_ + _)
    }
  }

  object If extends WFunc {
    def apply(e: Dict, args: List[Any]) = {
      require(args.size == 3)
      if (eval(e, args(0)).asInstanceOf[Boolean]) eval(e, args(1)) else eval(e, args(2))
    }
  }

  object Quote extends WFunc {
    def apply(e: Dict, args: List[Any]) = {
      require(args.size == 1)
      args.head // note: not eval'ing it
    }
  }

  object Vau extends WFunc {
    def apply(e: Dict, args: List[Any]) = {

      args match {
        case (eS: Symbol) :: (argsS: Symbol) :: code :: Nil => {
          require(!e.contains(eS))
          require(!e.contains(argsS))
          require(eS != argsS)
          new WFunc {
            def apply(de: Dict, args: List[Any]) = {
              val newEnv = e + (eS -> de) + (argsS -> args)
              eval(newEnv, code)
            }
          }
        }
        case a => sys.error("Unknown args for a lambda expression: " + a)
      }
    }
  }

  def foldReduce(op: (Int, Int) => Boolean, e: Dict, args: List[Any]): Boolean = {
    require(args.size > 1)
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
    def apply(e: Dict, args: List[Any]): Boolean = foldReduce(_ < _, e, args)
  }

  object LessThanOrEqual extends WFunc {
    def apply(e: Dict, args: List[Any]): Boolean = foldReduce(_ <= _, e, args)
  }

  object Equal extends WFunc {
    def apply(e: Dict, args: List[Any]): Boolean = foldReduce(_ == _, e, args)
  }

  object GreaterThan extends WFunc {
    def apply(e: Dict, args: List[Any]): Boolean = foldReduce(_ > _, e, args)
  }

  object GreaterThanOrEqual extends WFunc {
    def apply(e: Dict, args: List[Any]): Boolean = foldReduce(_ >= _, e, args)
  }

  object DoBlock extends WFunc {

    def run(e: Dict, forms: List[Any]): (Any, Dict) = {
      val (lets, rest) = forms.partition {
        _ match {
          case 'let :: (s: Symbol) :: v :: Nil => true
          case 'let :: _ => sys.error("Malformed let")
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

      val allLets = lets.map(x => { val y = x.asInstanceOf[List[Any]]; (y(1).asInstanceOf[Symbol] -> LetResult(y(2))) })

      // Now, let's add them all to a new environment

      val newEnv = allLets.foldLeft(e) {
        (oldEnv, form) =>
          require(!oldEnv.contains(form._1), "Can't redefine a symbol: " + form._1)
          oldEnv + form
      }

      // Now all our let's need a reference to the env they were defined in

      allLets.foreach { l => l._2.setEnv(newEnv) }

      // now that we done all our static environment stuff, we can go through and evaluate it all

      (rest.foldLeft(List(): Any)((a, b) => eval(newEnv, b)), newEnv)
    }

    def apply(e: Dict, forms: List[Any]): Any = {
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
      case (value: List[_]) :: (index: Int) :: Nil => value(index)
    }
  }

  object Cons extends StrictFunc {
    def run = {
      case h :: (tail: List[_]) :: Nil => h :: tail
    }
  }

  object Trace extends StrictFunc {
    def run = {
      case args => println(args.mkString)
    }
  }

  object Assert extends StrictFunc {
    def run = {
      case (res: Boolean) :: Nil => if (!res) sys.error("Code assertion failed!")
      case (res: Boolean) :: (msg: String) :: Nil => if (!res) sys.error("Code assertion failed, with errror: " + msg)
    }
  }

  object Length extends StrictFunc {
    def run = {
      case (l: List[_]) :: Nil => l.length
    }
  }

  object DictSize extends StrictFunc {
    def run = {
      case (lu: Dict) :: Nil => lu.size
    }
  }

  object DictGet extends StrictFunc {
    def run = {
      case (lu: Dict) :: k :: Nil => lu(k)
    }
  }

  // TODO: this should be able to be a StricTfunc, but we need access to 'e' to call 'f'
  object MapFunc extends WFunc {
    def apply(e: Dict, args: List[Any]): Any = {
      require(args.size == 2)
      val f = eval(e, args(0)).asInstanceOf[WFunc]
      val l = eval(e, args(1)).asInstanceOf[List[_]]

      l.map(x => f(e, x :: Nil))
    }
  }

  object FoldLeft extends WFunc {
    def apply(e: Dict, args: List[Any]): Any = {
      require(args.size == 3)
      val list = eval(e, args(0)).asInstanceOf[List[_]]
      val start = eval(e, args(1))
      val func = eval(e, args(1)).asInstanceOf[WFunc]

      list.foldLeft(start)((a, b) => func(e, a :: b :: Nil))
    }
  }

  object DictInsert extends StrictFunc {
    def run = {
      case (lu: Dict) :: k :: v :: Nil => {
        lu + (k -> v)
      }
    }
  }

  object DictRemove extends StrictFunc {
    def run = {
      case (lu: Dict) :: k :: nil => lu - k
    }
  }

  object DictContains extends StrictFunc {
    def run = {
      case (lu: Dict) :: k :: Nil => lu.contains(k)
    }
  }

  object Not extends StrictFunc {
    def run = {
      case (arg: Boolean) :: Nil => !arg
    }
  }

  object And extends WFunc {
    def apply(e: Dict, args: List[Any]): Boolean = {

      require(args.size >= 2)

      args.foreach { v =>
        if (eval(e, v).asInstanceOf[Boolean] == false)
          return false
      }

      true

    }
  }

  object Or extends WFunc {
    def apply(e: Dict, args: List[Any]): Boolean = {
      require(args.size >= 2)

      args.foreach { v =>
        if (eval(e, v).asInstanceOf[Boolean] == true)
          return true
      }

      false
    }
  }

}
