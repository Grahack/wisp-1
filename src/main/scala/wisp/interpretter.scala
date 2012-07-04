package wisp

object Interpretter {

  def eval[As](env: WEnv, arg: Any): As = (arg match {
    case s: Symbol => env(s)
    case head :: tail => eval[WFunc](env, head)(tail)
    case l: IsWList => sys.error("Trying to eval a list, but didn't match correct form")
    case x => x
  }).asInstanceOf[As]

  def eval[As](arg: Any): As = (arg match {
    case head :: tail => head.asInstanceOf[WFunc](tail)
    case l: IsWList => sys.error("Trying to eval a list, but didn't match correct form")
    case x => x
  }).asInstanceOf[As]

  def resolve(env: WEnv, in: Any): Any = in match {
    case s: Symbol => env(s)
    case x => x
  }

  def format(a: Any): String = {
    a match {
      case l: IsWList => l.map(format(_)).mkString("(", " ", ")")
      case s: Symbol => s.name
      case i: Int => i.toString
      case m: IsWMap => "(#map-new " + m.toList.map(x => "(" + format(x._1) + " " + format(x._2) + ")").mkString(" ") + ")"
      case b: WFunc => b.name.name
      case s: String => '"' + s + '"'
      case b: Boolean => if (b) "#true" else "#false"
      case _ => sys.error("Unknown type of: " + a)
    }
  }

  def summary(v: Any): String = {
    val r = format(v)
    if (r.length > 200) r.substring(0, 197) + "..." else r
  }

  def read(file: String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()
    lines
  }

  trait SimpleFunc extends WFunc {
    def apply(args: WList) = args match {
      case e :: rest => {
        val env = e.asInstanceOf[WEnv]
        run(rest.map(x => eval(env, x)))
      }
      case _ => err
    }

    def run(args: WList): Any
  }

  case class UserDefinedFunc(capEnv: WEnv, argName: Symbol, capBody: Any) extends WFunc {
    def name = Symbol("<UDF>")
    def apply(args: WList) = eval(capEnv + (argName -> args), capBody)
  }

  val builtinValues = List(
    // crazy primitives
    new WFunc {
      def name = Symbol("#$eval")
      def apply(args: WList) = {
        require(!args.isEmpty)

        // this is a little tricky, as it can be anything -- unless something is after it,
        // in which case it must be a WEnv
        val fst = eval[Any](args.head) 

        args.tail.foldLeft(fst)((e, n) => eval[Any](e.asInstanceOf[WEnv], n))
      }
    },

    new WFunc {
      def name = Symbol("#$lambda")
      def apply(args: WList) = args match {
        case (env: WEnv) :: (symbol: Symbol) :: body :: Nil => UserDefinedFunc(env, symbol, body)
        case _ => err
      }
    },

    new WFunc {
      def name = Symbol("#$let")
      def apply(args: WList) = args match {
        case (env: WEnv) :: key :: v :: Nil => env + (key -> v)
        case _ => err
      }
    },

    new WFunc {
      def name = Symbol("#$thread-first")
      def apply(args: WList) = args match {
        case single :: Nil => single
        case x :: (into: IsWList) :: rest => apply((into.head :: x :: into.tail.tail) :: rest)
        case _ => err
      }
    },

    // some crazy convenience stuff

    new WFunc {
      def name = Symbol("#$print")
      def apply(args: WList) = {
        println("~~~~DEBUG: " + format(args))
        List()
      }
    },

    // normal functions

    new WFunc {
      def name = Symbol("#print")
      def apply(args: WList) = {
        println(format(args))
        List()
      }
    },

    new SimpleFunc {
      def name = Symbol("#int-sum")
      def run(args: WList) = args.map(_.asInstanceOf[Int]).reduce(_ + _)
    },
    new SimpleFunc {
      def name = Symbol("#list-new")
      def run(args: WList) = args
    }).map(x => (x.name, x)).toMap +
    (Symbol("#true") -> true) +
    (Symbol("#empty-map") -> new WMap())

}
