package espringe.wisp.unit_tests

import org.specs2.mutable._
import espringe.wisp._
import scala.util.parsing.input.Positional
import scala.collection.immutable.HashMap
import java.io.PrintWriter

class ParserSpec extends Specification {

  import BuiltinFunctionNames._

  "The Parser" should {

    "actually able to load a real file" in {
      /* Writes the string to disk, then uses the reader to load it */

      val path = java.io.File.createTempFile("wisp_parser_test", ".wisp")
      val stream = new PrintWriter(path, "UTF8") //  path. //java.io.File.newBufferedWriter(path, Charset.forName("UTF-8"))
      stream.write("434")
      stream.close()

      Parser(scala.io.Source.fromFile(path)) must_== Seq(434)
      path.delete()
    }

    "be able to read a numbers" in {
      read("44") must_== 44
    }

    "be able to read a symbol" in {
      read("cat") must_== 'cat
    }

    "can read chars" in {
      read("~q") must_== 'q'
      read("~a ~b ~3 ~d ~e ~f") must_== 'a' -> Seq('b', '3', 'd', 'e', 'f')
    }

    "be able to read a string" in {
      read("\"soup\"") must_== Seq('s', 'o', 'u', 'p')
      read("\"\"") must_== Seq()
      read("\"tiger\"") must_== read("[~t ~i ~g ~e ~r]")
    }

    "work with quoted lists" in {
      read("[a b c]") must_== Seq('a, 'b, 'c)
      read("[a [b 4]]") must_== Seq('a, Seq('b, 4))
    }

    "read a dict" in {
      read("{}") must_== Map()
      read("{\"soup\" key}") must_== Map("soup" -> 'key)

      read("{key value \"dog\" 44 ~f 50}") must_== Map(
        'key -> 'value,
        Seq('d', 'o', 'g') -> 44,
        WChar('f') -> 50)
    }

    "handle explicit function calls / lists" in {
      read("(f a b)") must_== 'f -> Seq('a, 'b)
      read("(f a b)") must_== read("f a b")

      read("f.x") must_== 'f -> Seq('x)
      read("loco.34") must_== 'loco -> Seq(34)
      read("a f g.x") must_== read("a f (g x)")

      read("(f (a b) c)") must_== Sym('f) -> Seq('a -> Seq('b), 'c)
      read("(f a") must throwA[Throwable]
      read("f a a)") must throwA[Throwable]
      read("(f (a a)") must throwA[Throwable]
    }

    "work with leading/trailing slines" in {
      read("\ncat") must_== 'cat
      read("shield\n") must_== 'shield
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
      read("#num-add") must_== NumAdd
      read("#list-make") must_== ListMake
    }

    "read an empty file" in {
      Parser("").length must_== 0
    }

    "handle trailing comment, without a new line" in {
      read("4 ; comment") must_== 4
    }

  }

  def read(s: String) = {
    val r = Parser(s)
    assert(r.length == 1)
    r(0)
  }

}
