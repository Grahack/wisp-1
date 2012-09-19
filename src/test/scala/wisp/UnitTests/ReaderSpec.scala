package wisp.unit_tests

import org.specs2.mutable._
import wisp.Reader
import wisp.Vect
import wisp.Call


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

    "be able to read a number" in {
      Reader("44")._2 must_== 44
    }
    "be able to read a symbol" in {
      Reader("cat")._2 must_== 'cat
    }
    "be able to read a string" in {
      Reader("\"a string\"")._2 == "a string"
    }
    "read vectors" in {
      Reader("[a b c d]")._2 must_== Vect('a, 'b, 'c, 'd)
    }
    "handle top level function calls" in {
      Reader("func arg1 12 arg2")._2 must_== Call('func, Vect('arg1, 12, 'arg2))
    }
    "explicit function appliction" in {
      Reader("func(arg1 \"cat\")")._2 must_== Call('func, Vect('arg1, "cat"))
    }
  }

}
