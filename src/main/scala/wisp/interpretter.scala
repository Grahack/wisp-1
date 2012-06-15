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

  def name = Symbol("Func" + this)
}

object Interpretter {
  type Environment = scala.collection.immutable.HashMap[Any, Any]

  def resolveWithEnv(in: Any, env: Environment): (Any, Environment) = in match {
    case l: List[_] => resolve(l.head, env).asInstanceOf[BuiltinFunction](l.tail, env)
    case s: Symbol => (env(s), env)
    case x => (x, env)
  }
  def resolve(in: Any, env: Environment): Any = resolveWithEnv(in, env)._1

  def format(a: Any): String = {
    a match {
      case l: List[_] => l.map(format(_)).mkString("(", " ", ")")
      case s: Symbol => s.name
      case i: Int => i.toString
      case m: Map[_, _] => "(#map " + m.toList.map(x => "(" + format(x._1) + " " + format(x._2) + ")").mkString(" ") + ")"
      case b: BuiltinFunction => b.name.name
      case s: String => '"' + s + '"'
      case b: Boolean => if (b) "#true" else "#false"
      case _ => sys.error("Unknown type of: " + a)
    }
  }

  def summary(v: Any): String = {
    v match {
      case l: List[_] => "[List of " + l.length + "]"
      case s: Symbol => s.name
      case i: Int => i.toString
      case m: Map[_, _] => "[Map of " + m.size + "]"
      case b: BuiltinFunction => b.name.name
      case s: String => '"' + s.substring(0, 50) + '"' + (if (s.length >= 50) "..." else "")
      case b: Boolean => if (b) "#true" else "#false"
      case _ => sys.error("Unknown type of: " + v)
    }

  }

  val builtinFunctions = List(
    new BuiltinFunction {
      def name = Symbol("#define")
      def apply(args: List[Any], env: Environment): (Any, Environment) =
        args match {
          case symbol :: value :: Nil => {
            val s = symbol.asInstanceOf[Symbol]
            val r = resolve(value, env)
            (r, env + (s -> r))
          }
          case _ => sys.error("define expected 2 arguments, got: " + args + " instead")
        }
    },

    new BuiltinFunction {
      def name = Symbol("#eval")
      def apply(args: List[Any], env: Environment): (Any, Environment) = {

        val (body, e) = if (args.length == 2)
          (resolve(args(0), env), args(1).asInstanceOf[Environment])
        else if (args.length == 1)
          (resolve(args(0), env), env)
        else
          sys.error("eval expected either 1 or two arguments")

        resolveWithEnv(body, e)
      }
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
              resolveWithEnv(trueCase, env)
            else
              resolveWithEnv(falseCase, env)
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
            val source = scala.io.Source.fromFile(resolve(file, env).asInstanceOf[String])
            val lines = source.mkString
            source.close()

            (lines, env)
          }
          case _ => sys.error("#read expected 1 argument (filename), got: " + args)
        }
    },

    new BuiltinFunction {
      def name = Symbol("#parse")
      def apply(args: List[Any], env: Environment): (Any, Environment) = args match {
        case str :: Nil => {
          val source = Reader.parse(resolve(str, env).asInstanceOf[String])
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
      def name = Symbol("#head")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 1)
        (resolve(args.head, env).asInstanceOf[List[_]].head, env)
      }
    },

    new BuiltinFunction {
      def name = Symbol("#tail")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 1)
        (resolve(args.head, env).asInstanceOf[List[_]].tail, env)
      }
    },
    
    new BuiltinFunction {
      def name = Symbol("#empty?")
      def apply(args: List[Any], env: Environment) = {
        require(args.length == 1)
        (resolve(args.head, env).asInstanceOf[List[_]].isEmpty, env)
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
        println(args.map(resolve(_, env)).mkString(" "))
        (List(), env)
      }
    })
}
