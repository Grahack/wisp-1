package wisp

object Interpretter {

  type Env = Map[Symbol, Any]

  def preprocess(startingEnv: Env, v: List[Any]) = {

    val lets = v.flatMap {
      _ match {
        case Let(s, v) => Some(s -> Let(v))
        case _ => None
      }
    }

    // Now, let's add them all to a new environment

    val newEnv = lets.foldLeft(startingEnv) {
      (oldEnv, form) =>
        require(!oldEnv.contains(form._1), "Can't redefine a symbol: " + form._1)
        oldEnv + form
    }

    // Now, we're going to be really dirty -- and mutate the recently discovered lets, with the newEnvironment
    // (Note how this is a cyclic graph, and I don't feel like using functional hacks
    lets.map(_._2).filter(_.isInstanceOf[LazyStore]).foreach { _.asInstanceOf[LazyStore].setEnv(newEnv) }

    // now that we have an environment, lets go through..

    val forms: List[WVal] = v.map(processForm(newEnv, _))
    
    new Function0[Any] {
      def apply(): Any = forms.foldLeft(List(): Any)((a, b) => b.get())
    }
  }

  def processForm(e: Env, v: Any): WVal = {
    v match {
      case s: Symbol => {
        require(e.contains(s), "Unknown symbol " + s)
        processForm(e, e(s))
      }
      case Let(_, _) => WNone
      case Add(values) => Add(values.map(v => WInt(processForm(e, v))))
      case l: List[_] => sys.error("Unknown function call" + l)
      case x => WAny(x)
    }
  }

  object Add {
    def unapply(v: Any) = v match {
      case 'add :: args => Some(args)
      case _ => None
    }
    def apply(values: List[WInt]) = WIntFunc(() => values.map(_.get).reduce(_ + _))
  }

  object Let {
    def apply(form: Any) = form match {
      case l: List[_] => new LazyStore(form, false)
      case x => x
    }
    def unapply(v: Any) = v match {
      case 'let :: (s: Symbol) :: v :: Nil => Some(s, v)
      case _ => None
    }
  }

  class LazyStore(var payload: Any, var hasBeenEval: Boolean) extends WVal {

    def get(): Any = {

      if (!hasBeenEval) {
        hasBeenEval = true
        assert(payload.isInstanceOf[WVal])
        payload = payload.asInstanceOf[WVal].get()
      }
      payload
    }

    def setEnv(e: Env) {
      assert(!hasBeenEval)
      payload = processForm(e, payload)
    }

  }

}
