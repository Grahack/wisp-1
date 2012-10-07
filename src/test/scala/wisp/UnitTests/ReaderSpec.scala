package wisp.unit_tests

import org.specs2.mutable._
import wisp._
import scala.util.parsing.input.Positional

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

      434 must_== Reader(path)._2 // specs2 bug
    }

    "be able to read a numbers" in {
      44 must_== read("44")

      val pos = read("eat 189 chicken").asInstanceOf[AtomList].value(1).asInstanceOf[Positional].pos

      pos.line must_== 1
      pos.column must_== 5
    }
    "be able to read a symbol" in {
      'cat must_== read("cat")

      val pos = read("\nhi cat rabbit").asInstanceOf[AtomList].value(2).asInstanceOf[Positional].pos

      pos.line must_== 2
      pos.column must_== 8

    }
    "be able to read a string" in {
      List(ListMake, 's', 'o', 'u', 'p') must_== read("\"soup\"") 
      List(ListMake) must_== read("\"\"")
      
      val pos = read("100 \"robbers\"").asInstanceOf[AtomList].value(1).asInstanceOf[Positional].pos

      pos.line must_== 1
      pos.column must_== 5
    }
    "can read chars" in {
      'q' must_== read("~q")

      List('a', 'b', '3', 'd', 'e', 'f') must_== read("~a ~b ~3 ~d ~e ~f")
    }
    "work with convenience quoted lists" in {
      List(ListMake, 1, 2, 3) must_== read("[1 2 3]")
    }
    "handle explicit function calls / lists" in {
      List('f, 'a, 'b) must_== read("(f a b)")
      read("f a b") must_== read("(f a b)")

      List('f, 'x) must_== read("f.x")
      List('loco, 34) must_== read("loco.34")
      read("a f g.x") must_== read("a f (g x)")
      //read

      List('f, List('a, 'b), 'c) must_== read("(f (a b) c)")
      read("(f a ") must throwA
      read("f a a)") must throwA
      read("(f (a a)") must throwA
    }

    "work with leading/trailing slines" in {
      'cat must_== read("\ncat")
      'shield must_== read("shield\n")
    }

    "handle significant whitespace" in {
      read("func a b c") must_== read("(func a b c)")

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
  }

  def read(s: String) = Reader(s)._2

}
