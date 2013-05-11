//package espringe.wisp.unit_tests
//
//import org.specs2.mutable._
//import espringe.wisp._
//import scala.util.parsing.input.Positional
//import scala.collection.immutable.HashMap
//import java.io.PrintWriter
//
//class ParserSpec extends Specification {
//
//  "The Parser" should {
//
//    "actually able to load a real file" in {
//      /* Writes the string to disk, then uses the reader to load it */
//
//      val path = java.io.File.createTempFile("wisp_parser_test", ".wisp")
//      val stream = new PrintWriter(path, "UTF8") //  path. //java.io.File.newBufferedWriter(path, Charset.forName("UTF-8"))
//      stream.write("434")
//      stream.close()
//
//      Parser(scala.io.Source.fromFile(path)) must_== Seq(434)
//    }
//
//    "be able to read a numbers" in {
//      read("44") must_== 44
//
//      val pos = read("eat 189 chicken").asInstanceOf[WList].value(1).asInstanceOf[Positional].pos
//
//      pos.line must_== 1
//      pos.column must_== 5
//    }
//    "be able to read a symbol" in {
//      read("cat") must_== 'cat
//
//      val pos = read("\nhi cat rabbit").asInstanceOf[WList].value(2).asInstanceOf[Positional].pos
//
//      pos.line must_== 2
//      pos.column must_== 8
//
//    }
//    "be able to read a string" in {
// //     read("\"soup\"") must_== Seq(new ListMake {}, 's', 'o', 'u', 'p')
//  //    read("\"\"") must_== Seq(new ListMake {})
//      read("\"tiger\"") must_== read("[~t ~i ~g ~e ~r]")
//
//      val pos = read("100 \"robbers\"").asInstanceOf[WList].value(1).asInstanceOf[Positional].pos
//
//      pos.line must_== 1
//      pos.column must_== 5
//    }
//    "can read chars" in {
//      read("~q") must_== 'q'
//
//      read("~a ~b ~3 ~d ~e ~f") must_== Seq('a', 'b', '3', 'd', 'e', 'f')
//    }
//    "work with quoted lists" in {
//      read("[a b c]") must_== Seq(new ListMake {}, 'a, 'b, 'c)
//      read("[a [b 4]]") must_== Seq(new ListMake {}, 'a, Seq(new ListMake {}, 'b, 4))
//    }
//    "read a dict" in {
//      read("{}") must_== Seq(new DictMake {})
//      read("{\"soup\" key}") must_== Seq(new DictMake {}, Seq(new ListMake {}, Seq(new ListMake {}, 's', 'o', 'u', 'p'), 'key))
//      read("{key value, \"dog\" 44, ~f 50}") must_== Seq(new DictMake {},
//          Seq(new ListMake {}, 'key, 'value),
//          Seq(new ListMake {},  Seq(new ListMake {}, 'd', 'o', 'g'), 44),
//          Seq(new ListMake {}, 'f', 50))
//    }
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
//
//  }
//
//  def read(s: String) = {
//    val r = Parser(s)
//    assert(r.value.length == 1)
//    r.value(0)
//  }
//
//}
