package wisp

import Interpretter._

trait BuiltinFunction {
  def apply(env: Environment, args: List[Any]): (Environment, Any)
  def name: Symbol
}

class UserDefinedFunction(envSymbol: Symbol, capEnv: Environment, argSymbol: Symbol, body: Any) extends BuiltinFunction {
  def apply(env: Environment, args: List[Any]) = {
    val newEnv = (capEnv merge env) + (argSymbol -> args) + (envSymbol -> env)
    eval(newEnv, body)
  }

  def name = Symbol("Func" + hashCode)
}

object Builtin {

  val values: Map[Symbol,Any] = List(
    new BuiltinFunction {
      def name = Symbol("#set")
      def apply(env: Environment, args: List[Any]): (Environment, Any) =
        args match {
          case symbol :: value :: Nil => {
            val s = symbol.asInstanceOf[Symbol]
            val r = eval(env, value)._2 /// TODO:  hmmm should i keep the environment? ( i thikn it might be better ?)
            (env + (s -> r), r)
          }
          case _ => sys.error("define expected 2 arguments, got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#eval")
      def apply(env: Environment, args: List[Any]): (Environment, Any) = {
        args match {
          case e :: body :: nil => eval(e.asInstanceOf[Environment], body)
          case _ => sys.error("eval expected two arguments")
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#do")
      def apply(env: Environment, args: List[Any]) = evalBlock(env, args)
    },

    new BuiltinFunction {
      def name = Symbol("#get-env")
      def apply(env: Environment, args: List[Any]) = (env, env)
    },

    new BuiltinFunction {
      def name = Symbol("#if")
      def apply(env: Environment, args: List[Any]): (Environment, Any) =
        args match {
          case cond :: trueCase :: falseCase :: Nil => {
            if (eval(env, cond).asInstanceOf[Boolean])
              eval(env, trueCase)
            else
              eval(env, falseCase)
          }
          case _ => sys.error("if statement was expecting 3 args, found: " + args.length)
        }
    },

    new BuiltinFunction {
      def name = Symbol("#lambda")
      def apply(env: Environment, args: List[Any]): (Environment, Any) =
        args match {
          case e :: a :: body :: Nil => {
            val envSymbol = e.asInstanceOf[Symbol]
            val argSymbol = a.asInstanceOf[Symbol]

            (env, new UserDefinedFunction(envSymbol, env, argSymbol, body))
          }
          case _ => sys.error("lambda expected 3 arguments (args, env and body), got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#read")
      def apply(env: Environment, args: List[Any]): (Environment, Any) =
        args match {
          case file :: Nil => {
            (env, read(seval(env, file).asInstanceOf[String]))
          }
          case _ => sys.error("#read expected 1 argument (filename), got: " + args)
        }
    },

    new BuiltinFunction {
      def name = Symbol("#parse")
      def apply(env: Environment, args: List[Any]): (Environment, Any) = args match {
        case str :: Nil => {
          val source = Reader(seval(env, str).asInstanceOf[String])
          (env, source)
        }
        case _ => sys.error("#parse expected 1 argument (string to parse), got: " + args)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#let")
      def apply(env: Environment, args: List[Any]): (Environment, Any) =
        args match {
          case symbol :: value :: Nil => (env + (symbol.asInstanceOf[Symbol] -> value), value)
          case _ => sys.error("let expected 2 arguments, got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#set-env")
      def apply(env: Environment, args: List[Any]): (Environment, Any) =
        args match {
          case e :: Nil => {
            (seval(env, e).asInstanceOf[Environment], List())
          }
          case _ => sys.error("Wasn't expecting: " + args + " in retEnv")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#list-new")
      def apply(env: Environment, args: List[Any]) = {
        require(args.isEmpty)
        (env, List())
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-cons")
      def apply(env: Environment, args: List[Any]) =
        args match {
          case iten :: list :: nil => (env, seval(env, iten) :: seval(env, list).asInstanceOf[List[Any]])
          case _ => sys.error("Expected 2 arguments to cons")
        }

    },

    new BuiltinFunction {
      def name = Symbol("#list-first")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 1)
        (env, seval(env, args.head).asInstanceOf[List[Any]].head)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-rest")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 1)
        (env, seval(env, args.head).asInstanceOf[List[Any]].tail)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-empty?")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 1)
        (env, seval(env, args.head).asInstanceOf[List[Any]].isEmpty)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#map-new")
      def apply(env: Environment, args: List[Any]) = {
        require(args.isEmpty)
        (env, new HashMap())
      }
    },

    new BuiltinFunction {
      def name = Symbol("#plus")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => (env, seval(env, a).asInstanceOf[Int] + seval(env, b).asInstanceOf[Int])
          case _ => sys.error("#plus expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#minus")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => (env, seval(env, a).asInstanceOf[Int] - seval(env, b).asInstanceOf[Int])
          case _ => sys.error("#minus expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#multiply")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => (env, seval(env, a).asInstanceOf[Int] * seval(env, b).asInstanceOf[Int])
          case _ => sys.error("#multiply expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#divide")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => (env, seval(env, a).asInstanceOf[Int] / seval(env, b).asInstanceOf[Int])
          case _ => sys.error("#divide expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#less-than")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          // TODO: work on strings
          case a :: b :: Nil => (env, seval(env, a).asInstanceOf[Int] < seval(env, b).asInstanceOf[Int])
          case _ => sys.error("#equal expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#equal")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => (env, seval(env, a) == seval(env, b))
          case _ => sys.error("#equal expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#print")
      def apply(env: Environment, args: List[Any]) = {
        println(args.map(x => format(seval(env, x))).mkString(" "))
        (env, List())
      }
    }).map(x => (x.name, x)).toMap +
    (Symbol("#true") -> true) +
    (Symbol("#false") -> false) +
    (Symbol("#author") -> "Eric Springer")

}