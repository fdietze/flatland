package flatland.test

import flatland._

import org.scalatest._

class ArraySliceSpec extends FreeSpec with MustMatchers {

  "ArraySliceInt" - {
    "empty" in {
      val slice = ArraySliceInt(Array(), 0, 0)
      slice.isEmpty mustEqual true
    }
    "single element" in {
      val slice = ArraySliceInt(Array(7), 0, 1)
      slice.size mustEqual 1
      slice(0) mustEqual 7
      slice.toList mustEqual List(7)
    }
    "single element on larger array" in {
      val slice = ArraySliceInt(Array(2, 7, 5, 3, 4, 6), 3, 2)
      slice.size mustEqual 2
      slice(0) mustEqual 3
      slice(1) mustEqual 4
      slice.toList mustEqual List(3, 4)
    }
    "sub slice" in {
      val initial = ArraySliceInt(Array(2, 7, 5, 3, 4, 6), 1, 4)
      val slice = initial.slice(2, 4)
      slice.size mustEqual 2
      slice(0) mustEqual 3
      slice(1) mustEqual 4
      slice.toList mustEqual List(3, 4)
    }
    "filter" in {
      val slice = ArraySliceInt(Array(2, 7, 5, 3, 4, 6), 3, 2)
      val filtered = slice.filter(_ > 3)
      filtered.size mustEqual 1
      filtered(0) mustEqual 4
    }
    "is IndexedSeq" in {
      val slice = ArraySliceInt(Array(2, 7, 5, 3, 4, 6), 3, 2)
      slice.map(_.toString) mustEqual List("3","4")
    }
    "viewMapInt" in {
      val slice = ArraySliceInt(Array(2, 7, 5, 3, 4, 6), 3, 2)
      slice.viewMapInt(_ + 1).map(_.toString) mustEqual List("4","5")
    }
  }
}
