package espringe.wisp.unit_tests

import org.specs2.mutable._
import espringe.wisp._
import scala.util.parsing.input.Positional
import scala.collection.immutable.HashMap
import java.io.PrintWriter

class ParserSpec extends Specification {

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

      read("~a ~b ~3 ~d ~e ~f") must_== WList(Stream(WChar('a'), WChar('b'), WChar('3'), WChar('d'), WChar('e'), WChar('f')))
    }

    "be able to read a string" in {
      read("\"soup\"") must_== WList(Stream(BuiltinFunction(BuiltinFunctionNames.ListMake), WChar('s'), WChar('o'), WChar('u'), WChar('p')))
      read("\"\"") must_== WList(Stream(BuiltinFunction(BuiltinFunctionNames.ListMake)))
      read("\"tiger\"") must_== read("[~t ~i ~g ~e ~r]")
    }

    "work with quoted lists" in {
      read("[a b c]") must_== WList(Stream(BuiltinFunction(BuiltinFunctionNames.ListMake), Sym('a), Sym('b), Sym('c)))
      read("[a [b 4]]") must_== WList(Stream(BuiltinFunction(BuiltinFunctionNames.ListMake), Sym('a), WList(Stream(BuiltinFunction(BuiltinFunctionNames.ListMake), Sym('b), Num(4)))))
    }

    "read a dict" in {
      read("{}") must_== Seq(BuiltinFunction(BuiltinFunctionNames.DictMake))
      read("{\"soup\" key}") must_== Seq(new BuiltinFunction(BuiltinFunctionNames.DictMake) {}, Seq(BuiltinFunction(BuiltinFunctionNames.ListMake), Seq(BuiltinFunction(BuiltinFunctionNames.ListMake), 's', 'o', 'u', 'p'), 'key))
      read("{key value, \"dog\" 44, ~f 50}") must_== Seq(BuiltinFunction(BuiltinFunctionNames.DictMake),
        Seq(BuiltinFunction(BuiltinFunctionNames.ListMake), 'key, 'value),
        Seq(BuiltinFunction(BuiltinFunctionNames.ListMake), Seq(BuiltinFunction(BuiltinFunctionNames.ListMake), 'd', 'o', 'g'), 44),
        Seq(BuiltinFunction(BuiltinFunctionNames.ListMake), 'f', 50))
    }

    //    "handle explicit function calls / lists" in {
    //      read("(f a b)") must_== List('f, 'a, 'b)
    //      read("(f a b)") must_== read("f a b")
    //
    //      read("f.x") must_== List('f, 'x)
    //      read("loco.34") must_== List('loco, 34)
    //      read("a f g.x") must_== read("a f (g x)")
    //      //read
    //
    //      read("(f (a b) c)") must_== List('f, List('a, 'b), 'c)
    //      read("(f a ") must throwA[Throwable]
    //      read("f a a)") must throwA[Throwable]
    //      read("(f (a a)") must throwA[Throwable]
    //    }
    //
    //    "work with leading/trailing slines" in {
    //      read("\ncat") must_== 'cat
    //      read("shield\n") must_== 'shield
    //    }
    //
    //    "handle significant whitespace" in {
    //
    //      read("""
    //          |f
    //          |	a""".stripMargin) must_== read("(f a)")
    //
    //      read("""
    //    	 |f a b
    //    	 |	c
    //         |	d""".stripMargin) must_== read("(f a b c d)")
    //
    //      read("""
    //    	 |f a b ; a comment
    //    	 |	c.d e
    //         | ; i don't like this, but the comment is space-prefixed
    //         |
    //         |		f
    //         |		g h
    //         |	i""".stripMargin) must_== read("(f a b ((c d) e f (g h)) i)")
    //
    //    }
    //
    //    "reads builtins" in {
    //      read("#num-add") must beAnInstanceOf[NumAdd]
    //      read("#list-make") must beAnInstanceOf[ListMake]
    //
    //      val pos = read("\nListHead\n\tListHead").asInstanceOf[WList].value(1).asInstanceOf[Positional].pos
    //      pos.line must_== 3
    //      pos.column must_== 2
    //    }

  }

  def read(s: String) = {
    val r = Parser(s)
    assert(r.length == 1)
    r(0)
  }

}
