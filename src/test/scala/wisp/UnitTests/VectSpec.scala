package wisp.unit_tests

import org.specs2.mutable._
import wisp.Vect

class VectSpec extends Specification {

  "A Vect" should {

    "Should be able to accept varargs" in {
      Vect(1, 2, 3, 'rocket) == List(1, 2, 3, 'rocket)
    }
    "Work nestedly" in {
      Vect(Vect(), 'rocket) == List(List(), 'rocket)
    }
    "Have a correct length" in {
      Vect().length must_== 0
      Vect(10, 20, "cat-graver robber").length must_== 3
      Vect.fromSeq(1 to 800).length must_== 800

      (Vect.fromSeq(1 to 500).tail ++ Vect.fromSeq(1 to 500)).length must_== 999
    }
    "Convert to a string" in {
      Vect.fromSeq("Dinner is delicious".toList) == "Dinner is delicious"
    }
    
    "Getting a tail" in {
      
      val ft = Vect(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11).data.self
      
      println("ft size: " + ft.iterator.size)
      ft.pp
      
      ft.viewl.tail.pp
      
      ok
      
    }
    
  }

}
