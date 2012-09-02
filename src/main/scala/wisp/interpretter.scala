package wisp

import java.nio.file.Path
import java.nio.file.Paths

object Interpretter {

  def apply(path: Path): (Any, Env) = {

    val forms = Reader(path)

    val (imports, rest) = forms.span(_ match {
      case 'import :: _ => true
      case _ => false
    })

    val env = if (imports.isEmpty) startingEnv else {

      imports.foldLeft(Map[Symbol, Any]())((e, nextImport) => nextImport match {
        case 'import :: (importFilePath: String) :: Nil => {
          // TODO: avoid double-loading a file
          e ++ Interpretter(path.resolveSibling(importFilePath))._2
        }
        case _ => sys.error("Could not understand import")
      })

    }

    DoBlock.run(env, rest)
  }

  private def startingEnv: Env = Map[Symbol, Any](
    ('true -> true),
    ('false -> false),
    // Some pretty primitive stuff
    ('do -> DoBlock),
    ('eval -> Eval),
    ('if -> If),
    ('lambda -> Lambda),
    (Symbol("'") -> Quote),
    // some maths stuff
    (Symbol("+") -> Add),
    (Symbol("-") -> Sub),
    (Symbol("<") -> LessThan),
    (Symbol("<=") -> LessThanOrEqual),
    (Symbol("==") -> Equal),
    (Symbol(">") -> GreaterThan),
    (Symbol(">=") -> GreaterThanOrEqual),
    // utility like
    ('str -> Str),
    ('nth -> Nth),
    ('assert -> Assert),
    ('size -> Size),
    // debug
    ('trace -> Trace))

  trait WVal {
    def apply(e: Env): Any
  }

  trait WFunc {
    def apply(e: Env, args: List[Any]): Any
  }

  def eval(e: Env, form: Any): Any = {
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
    def apply(e: Env, args: List[Any]) = {
      require(args.size == 2)

      eval(
        eval(e, args(0)).asInstanceOf[Env],
        eval(e, args(1)))
    }
  }

  object Add extends WFunc {
    def apply(e: Env, args: List[Any]) = { args.foldLeft(0)((acc, b) => acc + eval(e, b).asInstanceOf[Int]) }
  }

  object Sub extends WFunc {
    def apply(e: Env, args: List[Any]) =
      args.size match {
        case 0 => 0
        case 1 => -eval(e, args.head).asInstanceOf[Int]
        case _ => args.tail.foldLeft((eval(e, args.head).asInstanceOf[Int]))((acc, b) => acc - eval(e, b).asInstanceOf[Int])
      }
  }

  object If extends WFunc {
    def apply(e: Env, args: List[Any]) = {
      require(args.size == 3)
      if (eval(e, args(0)).asInstanceOf[Boolean]) eval(e, args(1)) else eval(e, args(2))
    }
  }

  object Quote extends WFunc {
    def apply(e: Env, args: List[Any]) = {
      require(args.size == 1)
      args.head // note: not eval'ing it
    }
  }

  object Lambda extends WFunc {
    def apply(e: Env, args: List[Any]) = {

      args match {
        case (eS: Symbol) :: (argsS: Symbol) :: code :: Nil => {
          require(!e.contains(eS))
          require(!e.contains(argsS))
          require(eS != argsS)
          new WFunc {
            def apply(de: Env, args: List[Any]) = {
              val newEnv = e + (eS -> de) + (argsS -> args)
              eval(newEnv, code)
            }
          }
        }
        case a => sys.error("Unknown args for a lambda expression: " + a)
      }
    }
  }

  def foldReduce(op: (Int, Int) => Boolean, e: Env, args: List[Any]): Boolean = {
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
    def apply(e: Env, args: List[Any]): Boolean = foldReduce(_ < _, e, args)
  }

  object LessThanOrEqual extends WFunc {
    def apply(e: Env, args: List[Any]): Boolean = foldReduce(_ <= _, e, args)
  }

  object Equal extends WFunc {
    def apply(e: Env, args: List[Any]): Boolean = foldReduce(_ == _, e, args)
  }

  object GreaterThan extends WFunc {
    def apply(e: Env, args: List[Any]): Boolean = foldReduce(_ > _, e, args)
  }

  object GreaterThanOrEqual extends WFunc {
    def apply(e: Env, args: List[Any]): Boolean = foldReduce(_ >= _, e, args)
  }

  object DoBlock extends WFunc {

    def run(e: Env, forms: List[Any]): (Any, Env) = {
      val (lets, rest) = forms.partition {
        _ match {
          case 'let :: (s: Symbol) :: v :: Nil => true
          case 'let :: _ => sys.error("Malformed let")
          case _ => false
        }
      }

      object LetResult { def apply(v: Any) = new LetResult(v, false) }

      class LetResult(var payload: Any, var hasBeenEval: Boolean) extends WVal {

        def apply(u: Env): Any = {

          if (!hasBeenEval) {
            val (capEnv, original) = payload.asInstanceOf[(Env, Any)]
            payload = eval(capEnv, original)
            hasBeenEval = true
          }
          payload
        }
        def setEnv(e: Env) = {
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

    def apply(e: Env, forms: List[Any]): Any = {
      run(e, forms)._1
    }
  }

  object Str extends WFunc {
    // TODO: might need to use a string-builder
    def apply(e: Env, args: List[Any]): Any = args.map(eval(e, _).toString).reduce(_ + _)
  }

  object Nth extends WFunc {
    def apply(e: Env, args: List[Any]): Any = {
      require(args.length == 2)

      val value = eval(e, args(0)).asInstanceOf[List[_]]
      val index = eval(e, args(1)).asInstanceOf[Int]

      value(index)
    }
  }

  object Trace extends WFunc {
    def apply(e: Env, args: List[Any]): Any = {
      println(args.map(eval(e, _)).mkString)
    }
  }

  object Assert extends WFunc {
    def apply(e: Env, args: List[Any]): Any = {
      require(args.size == 1 || args.size == 2)
      val r = eval(e, args(0)).asInstanceOf[Boolean]
      if (!r) {
        sys.error("Code assertion failed!" + (if (args.size == 2) " Message is: " + eval(e, args(1))))
      }
    }
  }

  object Size extends WFunc {
    def apply(e: Env, args: List[Any]): Any = {
      require(args.size == 1)
      eval(e, args) match {
        case l: List[_] => l.size
        case m: Map[_, _] => m.size
      }
    }

  }

}
