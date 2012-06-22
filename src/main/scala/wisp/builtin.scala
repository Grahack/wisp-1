package wisp

import Interpretter._

trait BuiltinFunction {
  def apply(args: List[Any], env: Environment): (Any, Environment)
  def name: Symbol
}

class UserDefinedFunction(argSymbol: Symbol, envSymbol: Symbol, capEnv: Environment, body: Any) extends BuiltinFunction {
  def apply(args: List[Any], env: Environment) = {
    val newEnv = (capEnv merge env) + (argSymbol -> args) + (envSymbol -> env)
    (resolve(body, newEnv), env)
  }

  def name = Symbol("Func" + hashCode)
}

object Builtin {

  val values: Map[Symbol, Any] = List(
    new BuiltinFunction {
      def name = Symbol("#set")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case symbol :: value :: Nil => {
            val s = symbol.asInstanceOf[Symbol]
            val r = eval(value, env)._1 /// TODO:  hmmm should i keep the environment? ( i thikn it might be better ?)
            (r, env + (s -> r))
          }
          case _ => sys.error("define expected 2 arguments, got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#eval")
      def apply(args: List[Any], env: Environment): (Any, Environment) = {       
        args match {
          case body :: e :: nil => eval(body, e.asInstanceOf[Environment])
          case _ => sys.error("eval expected two arguments")
        }          
      }
    },
    
    new BuiltinFunction {
      def name = Symbol("#do")
      def apply(args: List[Any], env: Environment) = evalBlock(args, env)
    },
    

    new BuiltinFunction {
      def name = Symbol("#get-env")
      def apply(args: List[Any], env: Environment) = (env, env)
    },

    new BuiltinFunction {
      def name = Symbol("#if")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case cond :: trueCase :: falseCase :: Nil => {
            if (resolve(cond, env).asInstanceOf[Boolean])
              eval(trueCase, env)
            else
              eval(falseCase, env)
          }
          case _ => sys.error("if statement was expecting 3 args, found: " + args.length)
        }
    },

    new BuiltinFunction {
      def name = Symbol("#lambda")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case a :: e :: body :: Nil => {
            val argSymbol = a.asInstanceOf[Symbol]
            val envSymbol = a.asInstanceOf[Symbol]

            (new UserDefinedFunction(argSymbol, envSymbol, env, body), env)
          }
          case _ => sys.error("lambda expected 3 arguments (args, env and body), got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#read")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case file :: Nil => {
            (read(resolve(file, env).asInstanceOf[String]), env)
          }
          case _ => sys.error("#read expected 1 argument (filename), got: " + args)
        }
    },

    new BuiltinFunction {
      def name = Symbol("#parse")
      def apply(args: List[Any], env: Environment): (Any, Environment) = args match {
        case str :: Nil => {
          val source = Reader(resolve(str, env).asInstanceOf[String])
          (source, env)
        }
        case _ => sys.error("#parse expected 1 argument (string to parse), got: " + args)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#let")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case symbol :: value :: Nil => (value, env + (symbol.asInstanceOf[Symbol] -> value))
          case _ => sys.error("let expected 2 arguments, got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#set-env")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case e :: Nil => {
            (List(), resolve(e, env).asInstanceOf[Environment])
          }
          case _ => sys.error("Wasn't expecting: " + args + " in retEnv")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#list-empty")
      def apply(args: List[Any], env: Environment) = {
        require(args.isEmpty)
        (List(), env)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-add")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 2)
        val list = resolve(args.head, env).asInstanceOf[List[_]]
        val iten = resolve(args.tail, env)

        (iten :: list, env)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-first")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 1)
        (resolve(args.head, env).asInstanceOf[List[_]].head, env)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-rest")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 1)
        (resolve(args.head, env).asInstanceOf[List[_]].tail, env)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#list-empty?")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 1)
        (resolve(args.head, env).asInstanceOf[List[_]].isEmpty, env)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#map-empty")
      def apply(args: List[Any], env: Environment) = {
        require(args.isEmpty)
        (new Environment(), env) // NOTE: a map has the same type as environment ;D
      }
    },

    new BuiltinFunction {
      def name = Symbol("#plus")
      def apply(args: List[Any], env: Environment) = {
        args match {
          case a :: b :: Nil => (resolve(a, env).asInstanceOf[Int] + resolve(b, env).asInstanceOf[Int], env)
          case _ => sys.error("#plus expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#minus")
      def apply(args: List[Any], env: Environment) = {
        args match {
          case a :: b :: Nil => (resolve(a, env).asInstanceOf[Int] - resolve(b, env).asInstanceOf[Int], env)
          case _ => sys.error("#minus expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#multiply")
      def apply(args: List[Any], env: Environment) = {
        args match {
          case a :: b :: Nil => (resolve(a, env).asInstanceOf[Int] * resolve(b, env).asInstanceOf[Int], env)
          case _ => sys.error("#multiply expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#divide")
      def apply(args: List[Any], env: Environment) = {
        args match {
          case a :: b :: Nil => (resolve(a, env).asInstanceOf[Int] / resolve(b, env).asInstanceOf[Int], env)
          case _ => sys.error("#divide expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#less-than")
      def apply(args: List[Any], env: Environment) = {
        args match {
          // TODO: work on strings
          case a :: b :: Nil => (resolve(a, env).asInstanceOf[Int] < resolve(b, env).asInstanceOf[Int], env)
          case _ => sys.error("#equal expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#equal")
      def apply(args: List[Any], env: Environment) = {
        args match {
          case a :: b :: Nil => (resolve(a, env) == resolve(b, env), env)
          case _ => sys.error("#equal expected two arguments, found: " + args)
        }
      }
    },

    new BuiltinFunction {
      def name = Symbol("#print")
      def apply(args: List[Any], env: Environment) = {
        println(args.map(x => format(resolve(x, env))).mkString(" "))
        (List(), env)
      }
    }).map(x => (x.name, x)).toMap +
    (Symbol("#true") -> true) +
    (Symbol("#false") -> false) +
    (Symbol("#author") -> "Eric Springer")

}