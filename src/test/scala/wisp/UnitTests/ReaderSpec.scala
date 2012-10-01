package wisp.unit_tests

import org.specs2.mutable._
import wisp._

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

      Reader(path)._2 must_== 434
    }

    "be able to read a numbers" in {
      read("44") must_== 44
    }
    "be able to read a symbol" in {
      read("cat") must_== 'cat
    }
    "be able to read a string" in {
      read("\"soup\"") must_== Vect(WFunc.VectMake, 's', 'o', 'u', 'p')
      read("\"\"") must_== Vect(WFunc.VectMake)
    }
    "can read chars" in {
      read("'q") must_== 'q'

      read("'a 'b '3 'd 'e 'f") must_== Vect('a', 'b', '3', 'd', 'e', 'f')
    }
    "handle explicit function calls" in {
      read("(f a b)") must_== Vect('f, 'a, 'b)
      read("(f (a b) c)") must_== Vect('f, Vect('a, 'b), 'c)
      read("(f a ") must throwA
      read("f a a)") must throwA
      read("(f (a a)") must throwA
    }

    "work with leading/trailing lines" in {
      read("\ncat") must_== 'cat
      read("shield\n") must_== 'shield
    }

    "handle implicit function calls" in {
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
    	 |	c d e
         | ; i don't like this, but the comment is space-prefixed
         |		f
         |		g h
         |	i""".stripMargin) must_== read("(f a b (c d e f (g h) i))")

    }
  }

  def read(s: String) = Reader(s)._2

}
