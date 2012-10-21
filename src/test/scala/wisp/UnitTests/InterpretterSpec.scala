package wisp.unit_tests

import org.specs2.mutable._
import wisp._
import scala.util.parsing.input.Positional

class InterpretterSpec extends Specification {

  "The Interpretter" should {

    "Return values untouched" in {
      interpret("3") must_== 3
      interpret("#True") must_== true
      interpret("#False") must_== false
    }
    
    "Work with quotes things" in {
      interpret("\"roller\"") must_== "roller"
      interpret("[1 2 3 4 5]") must_== Seq(1, 2, 3, 4, 5)
      interpret("[1 2 [3 4 5]]") must_== Seq(1, 2, Seq(3, 4, 5))
      interpret("#quote 4") must_== 4
      interpret("#quote cat") must_== 'cat
      interpret("#quote (2 for the money)") must_== Seq(2, 'for, 'the, 'money)
    }
    
    "Basic function calls works" in {
      interpret("#num-add 1 2") must_== 3
    }
    
    "Nested function calls works" in {
      interpret("#num-add 1 (#num-add 5 2)") must_== 8
    }
  }

  def interpret(s: String) = Interpretter(Parser(s).value(0))

}
