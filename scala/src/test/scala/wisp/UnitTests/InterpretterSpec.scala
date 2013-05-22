package wisp.unit_tests

import org.specs2.mutable._
import espringe.wisp._
import scala.util.parsing.input.Positional

class InterpretterSpec extends Specification {

  "The Interpretter" should {

    "Return values untouched" in {
      interpret("3") must_== 3
      interpret("#true") must_== true
      interpret("#false") must_== false
      interpret("\"a string\"") must_== "a string"
    }

    "Work with quoted things" in {
      interpret("\"roller\"") must_== Seq('r', 'o', 'l', 'l', 'e', 'r')
      interpret("[1 2 3 4 5]") must_== Seq(1, 2, 3, 4, 5)
      interpret("[1 2 [3 4 5]]") must_== Seq(1, 2, Seq(3, 4, 5))
      interpret("#quote 4") must_== 4
      interpret("#quote cat") must_== 'cat
      interpret("#quote (sqrt 4)") must_== 'sqrt -> Seq(4)
      interpret("#quote [2 for the money]") must_== Seq(2, 'for, 'the, 'money)
    }

    "Basic function calls works" in {
      interpret("#num-add 1 2") must_== 3
    }

    "Nested function calls works" in {
      interpret("#num-add 1 (#num-add 5 2)") must_== 8
    }

    "Can parse a string" in {
      interpret("#parse \"hello\"") must_== Seq('hello)
    }

  }

  private val interpretter = new Interpretter(null)
  def interpret(s: String) = interpretter(Parser(s)(0))

}
