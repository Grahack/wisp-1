package wisp.unit_tests

import org.specs2.mutable._
import wisp.Vect

class VectSpec extends Specification { sequential

  "A Vect" should {

    "Should be able to accept varargs" in {
      Vect(1, 2, 3, 'rocket) == List(1, 2, 3, 'rocket)
    }
    "Work nestedly" in {
      Vect(Vect(), 'rocket) == List(List(), 'rocket)
    }
    "Have a correct length" in {
      Vect().length must_== 0
      Vect(10, 20, Vector(444, 88), "cat-graver robber").length must_== 4
      (Vect.fromSeq(1 to 500).tail ++ Vect.fromSeq(1 to 500)).length must_== 999
    }
    "Convert to a string" in {
      Vect.fromSeq("Dinner is delicious".toList) == "Dinner is delicious"
    }
    
    
  }

}
