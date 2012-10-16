package wisp.unit_tests

import org.specs2.mutable._
import wisp._
import scala.util.parsing.input.Positional
import scala.collection.immutable.HashMap

class ReaderSpec extends Specification {

  "The Reader" should {

    "actually able to load a real file" in {
      /* Writes the string to disk, then uses the reader to load it */

      import java.nio.file.Files
      import java.io.FileOutputStream
      import java.nio.charset.Charset
      val path = Files.createTempFile("wisp_reader_test", ".wisp")
      val stream = Files.newBufferedWriter(path, Charset.forName("UTF-8"))
      stream.write("434")
      stream.close()

      Reader(path)(0) must_== 434
    }

    "be able to read a numbers" in {
      read("44") must_== 44

      val pos = read("eat 189 chicken").asInstanceOf[WList].value(1).asInstanceOf[Positional].pos

      pos.line must_== 1
      pos.column must_== 5
    }
    "be able to read a symbol" in {
      read("cat") must_== 'cat

      val pos = read("\nhi cat rabbit").asInstanceOf[WList].value(2).asInstanceOf[Positional].pos

      pos.line must_== 2
      pos.column must_== 8

    }
    "be able to read a string" in {
      read("\"soup\"") must_== "soup"
      read("\"\"") must_== ""
      read("\"tiger\"") must_== read("[~t ~i ~g ~e ~r]")

      val pos = read("100 \"robbers\"").asInstanceOf[WList].value(1).asInstanceOf[Positional].pos

      pos.line must_== 1
      pos.column must_== 5
    }
    "can read chars" in {
      read("~q") must_== 'q'

      read("~a ~b ~3 ~d ~e ~f") must_== List('a', 'b', '3', 'd', 'e', 'f')
    }
    "work with vectors" in {
      read("[a b c]") must_== Seq('a, 'b, 'c)
      read("[a [b 4]]") must_== Seq('a, Seq('b, 4))
    }
    "read a dict" in {
      read("{}") must_== HashMap()
      read("{key value}") must_== HashMap('key -> 'value)
      read("{key value, 3 ~c, \"cat\" 44}") must_== HashMap('key -> 'value, 3 -> 'c', "cat" -> 44)
      //read("{key value, 3 ~c, \"cat\" 44}") must_== HashMap('key -> 'value, 3 -> 'c', "cat" -> 44)
    }
    "handle explicit function calls / lists" in {
      read("(f a b)") must_== List('f, 'a, 'b)
      read("(f a b)") must_== read("f a b")

      read("f.x") must_== List('f, 'x)
      read("loco.34") must_== List('loco, 34)
      read("a f g.x") must_== read("a f (g x)")
      //read

      read("(f (a b) c)") must_== List('f, List('a, 'b), 'c)
      read("(f a ") must throwA
      read("f a a)") must throwA
      read("(f (a a)") must throwA
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
      read("#num-add") must beAnInstanceOf[NumAdd]
      read("#list-make") must beAnInstanceOf[ListMake]

      val pos = read("\n#ListHead\n\t#ListHead").asInstanceOf[WList].value(1).asInstanceOf[Positional].pos
      pos.line must_== 3
      pos.column must_== 2
    }

  }

  def read(s: String) = Reader(s)(0)

}
