import org.specs2.mutable._
import wisp.Reader
import java.io.FileOutputStream
import java.nio.charset.Charset

import wisp._

import scalaz.IndSeq

class ReaderSpec extends Specification {

  implicit def annoyingPimp(a: Any) = new Object {
    def str_==(b: String) = a.asInstanceOf[Vect].convertToString.get must_== b
  }

  "The Reader" should {
    "be able to read a number" in {
      read("44")._2 must_== 44
    }
    "be able to read a symbol" in {
      read("cat")._2 must_== 'cat
    }
    "be able to read a string" in {
      read("\"a string\"")._2 str_== "a string"
    }
    "read vectors" in {
      read("[a b c d]")._2 must_== Vect('a, 'b, 'c, 'd)
    }
    "handle top level function calls" in {
      read("func arg1 arg2")._2 must_== Call('func, Vect('arg1, 'arg2))
    }
  }

  /* Writes the string to disk, then uses the reader to load it */
  def read(s: String) = {
    import java.nio.file.Files

    val path = Files.createTempFile("wisp_reader_test", ".wisp")

    val stream = Files.newBufferedWriter(path, Charset.forName("UTF-8"))
    stream.write(s)
    stream.close()

    Reader(path)
  }
}
