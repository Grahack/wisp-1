package wisp

object Interpretter {

  def apply(forms: List[Any]): Any = DoBlock(startingEnv, forms)

  private def startingEnv: Env = new Env() +
    ('true -> true) +
    ('false -> false) +
    // Some pretty primitive stuff
    ('do -> DoBlock) +
    ('eval -> Eval) +
    ('if -> If) +
    ('lambda -> Lambda) +
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
    // debug
    ('trace -> Trace)

  trait WVal {
    def apply(e: Env): Any
  }

  trait WFunc {
    def apply(e: Env, args: List[Any]): Any
  }

  def eval(e: Env, form: Any): Any = {
    assert(!e.contains('let))
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
    def apply(e: Env, args: List[Any]) = eval(e, args)
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

  object Lambda extends WFunc {
    def apply(e: Env, args: List[Any]) = {

      args match {
        case (argsS: Symbol) :: code :: Nil => {
          require(!e.contains(argsS))
          new WFunc {
            def apply(de: Env, args: List[Any]) = {
              val newEnv = e + (argsS -> args.map(eval(de, _)))

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
    def apply(e: Env, forms: List[Any]): Any = {
      val (lets, rest) = forms.partition {
        _ match {
          case 'let :: (s: Symbol) :: v :: Nil => true
          case 'let :: _ => sys.error("Malformed let")
          case _ => false
        }
      }

      object LetResult { def apply(v: Any) = new LetResult(v, false) }

      class LetResult(var value: Any, var hasBeenEval: Boolean) extends WVal {
        def apply(e: Env): Any = {
          if (!hasBeenEval) {
            value = eval(e, value)
            hasBeenEval = true
          }
          value
        }
      }

      val allLets = lets.map(x => { val y = x.asInstanceOf[List[Any]]; (y(1).asInstanceOf[Symbol] -> LetResult(y(2))) })

      // Now, let's add them all to a new environment

      val newEnv = allLets.foldLeft(e) {
        (oldEnv, form) =>
          require(!oldEnv.contains(form._1), "Can't redefine a symbol: " + form._1)
          oldEnv + form
      }

      // now that we done all our static environment stuff, we can go through and evaluate it all

      rest.foldLeft(List(): Any)((a, b) => eval(newEnv, b))
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

}
