package espringe.wisp.unit_tests

import org.specs2.mutable._
import espringe.wisp._
import scala.util.parsing.input.Positional
import scala.collection.immutable.HashMap
import java.io.PrintWriter

class ParserSpec extends Specification {

  import BuiltinFunctionNames._
  def bf(bf: BuiltinFunctionNames.Name) = BuiltinFunction(bf)
  def l(values: W*) = WList(values.toStream)

  "The Parser" should {

    "actually able to load a real file" in {
      /* Writes the string to disk, then uses the reader to load it */

      val path = java.io.File.createTempFile("wisp_parser_test", ".wisp")
      val stream = new PrintWriter(path, "UTF8") //  path. //java.io.File.newBufferedWriter(path, Charset.forName("UTF-8"))
      stream.write("434")
      stream.close()

      Parser(scala.io.Source.fromFile(path)) must_== Seq(Num(434))
    }

    "be able to read a numbers" in {
      read("44") must_== Num(44)
    }

    "be able to read a symbol" in {
      read("cat") must_== Sym('cat)
    }

    "can read chars" in {
      read("~q") must_== WChar('q')

      read("~a ~b ~3 ~d ~e ~f") must_== l(WChar('a'), WChar('b'), WChar('3'), WChar('d'), WChar('e'), WChar('f'))
    }

    "be able to read a string" in {
      read("\"soup\"") must_== l(bf(ListMake), WChar('s'), WChar('o'), WChar('u'), WChar('p'))
      read("\"\"") must_== l(bf(ListMake))
      read("\"tiger\"") must_== read("[~t ~i ~g ~e ~r]")
    }

    "work with quoted lists" in {
      read("[a b c]") must_== l(bf(ListMake), Sym('a), Sym('b), Sym('c))
      read("[a [b 4]]") must_== l(bf(ListMake), Sym('a), l(bf(ListMake), Sym('b), Num(4)))
    }

    "read a dict" in {
      read("{}") must_== l(bf(DictMake))
      read("{\"soup\" key}") must_== l(
        bf(DictMake), l(bf(ListMake),
          l(bf(ListMake), WChar('s'), WChar('o'), WChar('u'), WChar('p')), Sym('key)))

      read("{key value, \"dog\" 44, ~f 50}") must_== l(bf(DictMake),
        l(bf(ListMake), Sym('key), Sym('value)),
        l(bf(ListMake), l(bf(ListMake), WChar('d'), WChar('o'), WChar('g')), Num(44)),
        l(bf(ListMake), WChar('f'), Num(50)))
    }

    "handle explicit function calls / lists" in {
      read("(f a b)") must_== l(Sym('f), Sym('a), Sym('b))
      read("(f a b)") must_== read("f a b")

      read("f.x") must_== l(Sym('f), Sym('x))
      read("loco.34") must_== l(Sym('loco), Num(34))
      read("a f g.x") must_== read("a f (g x)")


      read("(f (a b) c)") must_== l(Sym('f), l(Sym('a), Sym('b)), Sym('c))
      read("(f a") must throwA[Throwable]
      read("f a a)") must throwA[Throwable]
      read("(f (a a)") must throwA[Throwable]
    }

    "work with leading/trailing slines" in {
      read("\ncat") must_== Sym('cat)
      read("shield\n") must_== Sym('shield)
    }

    "handle significant whitespace" in {

      read("""
              |f
              |	a""".stripMargin) must_== read("(f a)")

      read("""
        	 |f a b
        	 |	c
             |	d""".stripMargin) must_== read("(f a b c d)")

      read("""
        	 |f a b ; a comment
        	 |	c.d e
             | ; i don't like this, but the comment is space-prefixed
             |
             |		f
             |		g h
             |	i""".stripMargin) must_== read("(f a b ((c d) e f (g h)) i)")

    }

    "reads builtins" in {
      read("#num-add").asInstanceOf[BuiltinFunction].which must_== NumAdd
      read("#list-make").asInstanceOf[BuiltinFunction].which must_== ListMake
    }

  }

  def read(s: String) = {
    val r = Parser(s)
    assert(r.length == 1)
    r(0)
  }

}
