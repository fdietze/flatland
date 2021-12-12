package flatland.test

import flatland._

import org.scalatest._
import collection.mutable
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class IndexedSeqExtensionSpec extends AnyFreeSpec with Matchers {

  "IndexedSeq Extension methods" - {
    "foreachIndex" in {
      val array  = IndexedSeq(7, 8, 9)
      val buffer = new mutable.ArrayBuffer[Int]
      array.foreachIndex(buffer += _)
      buffer.toList mustEqual List(0, 1, 2)
    }

    "forallIndexAndElement" in {
      val array = IndexedSeq(7, 8, 9)
      array.forallIndexAndElement((i, elem) => i >= 0 && i < 3) mustEqual true
      array.forallIndexAndElement((i, elem) => i == 2) mustEqual false
      array.forallIndexAndElement((i, elem) => elem > 6) mustEqual true
      array.forallIndexAndElement((i, elem) => elem < 6) mustEqual false
    }
  }
}
