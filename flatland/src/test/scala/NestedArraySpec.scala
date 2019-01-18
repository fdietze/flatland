package flatland.test

import flatland._

import org.scalatest._

class NestedArraySpec extends FreeSpec with MustMatchers {

  "NestedArrayInt" - {
    "empty" in {
      val nested = NestedArrayInt(Array[Array[Int]]())
      nested.isEmpty mustEqual true
      nested.length mustEqual 0
      assertThrows[AnyRef](nested(0)) // should be IndexOutOfBoundsException, but scalajs thows UndefinedBehaviorException
    }
    "one empty array" in {
      val nested = NestedArrayInt(Array(Array[Int]()))
      nested.isEmpty mustEqual false
      nested.length mustEqual 1
      nested(0).isEmpty mustEqual true
    }
    "one single-element array" in {
      val nested = NestedArrayInt(Array(Array(13)))
      nested.length mustEqual 1
      nested(0)(0) mustEqual 13
      nested(0, 0) mustEqual 13
    }
    "two non-empty arrays" in {
      val nested = NestedArrayInt(Array(Array(7, 8, 9), Array(1, 2, 3)))
      nested.length mustEqual 2
      nested(0, 1) mustEqual 8
      nested(0)(1) mustEqual 8
      nested(1, 1) mustEqual 2
      nested(1)(1) mustEqual 2
      nested(0).toList mustEqual List(7, 8, 9)
      nested(1).toList mustEqual List(1, 2, 3)
    }
    "many arrays, some empty" in {
      val nested = NestedArrayInt(Array(Array(3), Array[Int](), Array(0), Array[Int](), Array(0, 1)))
      nested.length mustEqual 5
      nested(0).toList mustEqual List(3)
      nested(1).toList mustEqual List()
      nested(2).toList mustEqual List(0)
      nested(3).toList mustEqual List()
      nested(4).toList mustEqual List(0, 1)
    }
    "many arrays, some empty - from builders" in {
      def builder(elems: Int*) = {
        val b = new scala.collection.mutable.ArrayBuilder.ofInt
        b ++= elems
        b
      }
      val nested = NestedArrayInt(Array(builder(3), builder(), builder(0), builder(), builder(0, 1)))
      nested.length mustEqual 5
      nested(0).toList mustEqual List(3)
      nested(1).toList mustEqual List()
      nested(2).toList mustEqual List(0)
      nested(3).toList mustEqual List()
      nested(4).toList mustEqual List(0, 1)
    }

    "anyContains" in {
      val nested = NestedArrayInt(Array(Array(7, 8, 9), Array(1, 2, 3)))
      nested.anyContains(7) mustBe true
      nested.anyContains(8) mustBe true
      nested.anyContains(3) mustBe true
      nested.anyContains(4) mustBe false
    }

    "map" in {
      val nested = NestedArrayInt(Array(Array(7, 8, 9), Array(1, 2, 3)))
      nested.map(0)(_.toString).toList mustEqual List("7", "8", "9")
      nested.map(1)(identity).toList mustEqual List(1, 2, 3)
    }
  }
}
