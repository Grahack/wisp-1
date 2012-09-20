package wisp.unit_tests

import org.specs2.mutable._
import wisp._

class ReaderSpec extends Specification {
  sequential

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

    "be able to read a number" in {
      Reader("44")._2 must_== 44
    }
    "be able to read a symbol" in {
      Reader("cat")._2 must_== 'cat
    }
    "be able to read a string" in {
      Reader("\"soup\"")._2 must_== Vect(Quote, 's', 'o', 'u', 'p')
    }
    "can read chars" in {
      Reader("~q")._2 must_== 'q'

      Reader("~a ~b ~c ~d ~e ~f")._2 must_== Vect('a', 'b', 'c', 'd', 'e', 'f')
    }
    "read vectors" in {
      Reader("[a b c d]")._2 == Vect(Quote, 'a, 'b, 'c, 'd)
    }
    "work with quoted symbols" in {
      Reader("'robin")._2 == Vector(Quote, 'robin)

      val r = Reader("'robin")._2 == Vector(Quote, 'robin)
      r.pp
      ok
      // r
    }
    "handle top level function calls" in {
      Reader("func arg1 12 arg2")._2 must_== Vect('func, 'arg1, 12, 'arg2)
    }
    "explicit function appliction" in {
      Reader("""(func 65 arg1 44)""")._2 must_== Vect('func, 65, 'arg1, 44)
    }
  }

}
