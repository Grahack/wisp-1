package wisp

import Interpretter._

trait Function {
  def apply(env: Environment, args: List[Any]): Any
  def name: Symbol
}

class UserDefinedFunction(argSymbol: Symbol, envSymbol: Symbol, capEnv: Environment, body: Any) extends Function {
  def apply(env: Environment, args: List[Any]) = {
    val newEnv = (capEnv merge env) + (argSymbol -> args) + (envSymbol -> env)
    eval(newEnv, body)
  }

  def name = Symbol("Func" + hashCode)
}

object Builtin {

  val values: Map[Symbol, Any] = List(
    new Function {
      def name = Symbol("#define")
      def apply(env: Environment, args: List[Any]) =
        args match {
          case symbol :: value :: Nil => {
            env + (symbol -> eval(env, value))
          }
          case _ => sys.error("define expected 2 arguments, got: " + args + " instead")
        }
    },

    new Function {
      def name = Symbol("#eval")
      def apply(env: Environment, args: List[Any]): Any = {
        args match {
          case e :: b :: nil => {
            val evalEnv = eval(env, e).asInstanceOf[Environment]
            eval(evalEnv, b) // TODO: return env ?
          }
          case _ => sys.error("eval expected two arguments")
        }
      }
    },

    new Function {
      def name = Symbol("#run")
      def apply(env: Environment, args: List[Any]) = {
        println("Calling run: ")
        args.map(eval(env, _))
        env
      }
    },

    new Function {
      def name = Symbol("#if")
      def apply(env: Environment, args: List[Any]): Any =
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

    new Function {
      def name = Symbol("#lambda")
      def apply(env: Environment, args: List[Any]) =
        args match {
          case a :: e :: body :: Nil => {
            val argSymbol = a.asInstanceOf[Symbol]
            val envSymbol = e.asInstanceOf[Symbol]

            new UserDefinedFunction(argSymbol, envSymbol, env, body)
          }
          case _ => sys.error("lambda expected 3 arguments (args, env and body), got: " + args + " instead")
        }
    },

    new Function {
      def name = Symbol("#read")
      def apply(env: Environment, args: List[Any]) =
        args match {
          case file :: Nil => {
            read(eval(env, file).asInstanceOf[String])
          }
          case _ => sys.error("#read expected 1 argument (filename), got: " + args)
        }
    },

    new Function {
      def name = Symbol("#parse")
      def apply(env: Environment, args: List[Any]) = args match {
        case str :: Nil => {
          val source = Reader(eval(env, str).asInstanceOf[String])
          source
        }
        case _ => sys.error("#parse expected 1 argument (string to parse), got: " + args)
      }
    },

    new Function {
      def name = Symbol("#let")
      def apply(env: Environment, args: List[Any]) =
        args match {
          case symbol :: value :: Nil => env + (symbol.asInstanceOf[Symbol] -> value)
          case _ => sys.error("let expected 2 arguments, got: " + args + " instead")
        }
    },

    new Function {
      def name = Symbol("#list-new")
      def apply(env: Environment, args: List[Any]) = {
        require(args.isEmpty)
        List()
      }
    },

    new Function {
      def name = Symbol("#list-add")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 2)
        val list = eval(env, args.head).asInstanceOf[List[_]]
        val iten = resolve(env, args.tail)

        iten :: list
      }
    },

    new Function {
      def name = Symbol("#list-first")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 1)
        resolve(env, args.head).asInstanceOf[List[_]].head
      }
    },

    new Function {
      def name = Symbol("#list-rest")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 1)
        resolve(env, args.head).asInstanceOf[List[_]].tail
      }
    },

    new Function {
      def name = Symbol("#list-empty?")
      def apply(env: Environment, args: List[Any]) = {
        require(args.length == 1)
        resolve(env, args.head).asInstanceOf[List[_]].isEmpty
      }
    },

    new Function {
      def name = Symbol("#map-empty")
      def apply(env: Environment, args: List[Any]) = {
        require(args.isEmpty)
        new Environment() // NOTE: a map has the same type as environment ;D
      }
    },

    new Function {
      def name = Symbol("#plus")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => eval(env, a).asInstanceOf[Int] + eval(env, b).asInstanceOf[Int]
          case _ => sys.error("#plus expected two arguments, found: " + args)
        }
      }
    },

    new Function {
      def name = Symbol("#minus")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => eval(env, a).asInstanceOf[Int] - eval(env, b).asInstanceOf[Int]
          case _ => sys.error("#minus expected two arguments, found: " + args)
        }
      }
    },

    new Function {
      def name = Symbol("#multiply")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => eval(env, a).asInstanceOf[Int] * env(env, b).asInstanceOf[Int]
          case _ => sys.error("#multiply expected two arguments, found: " + args)
        }
      }
    },

    new Function {
      def name = Symbol("#divide")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => eval(env, a).asInstanceOf[Int] / eval(env, b).asInstanceOf[Int]
          case _ => sys.error("#divide expected two arguments, found: " + args)
        }
      }
    },

    new Function {
      def name = Symbol("#less-than")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          // TODO: work on strings
          case a :: b :: Nil => eval(env, a).asInstanceOf[Int] < eval(env, b).asInstanceOf[Int]
          case _ => sys.error("#equal expected two arguments, found: " + args)
        }
      }
    },

    new Function {
      def name = Symbol("#equal")
      def apply(env: Environment, args: List[Any]) = {
        args match {
          case a :: b :: Nil => eval(env, a) == eval(env, b)
          case _ => sys.error("#equal expected two arguments, found: " + args)
        }
      }
    },

    new Function {
      def name = Symbol("#print")
      def apply(env: Environment, args: List[Any]) = {
        println(args.map(x => format(eval(env, x))).mkString(" "))
        List()
      }
    }).map(x => (x.name, x)).toMap +
    (Symbol("#true") -> true) +
    (Symbol("#false") -> false) +
    (Symbol("#author") -> "Eric Springer")

}