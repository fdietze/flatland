package flatland.test

import flatland._

import org.scalatest._
import collection.mutable
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ArrayExtensionSpec extends AnyFreeSpec with Matchers {

  "Array Extension methods" - {
    "foreachIndex" in {
      val array = Array(7, 8, 9)
      val buffer = new mutable.ArrayBuffer[Int]
      array.foreachIndex(buffer += _)
      buffer.toList mustEqual List(0, 1, 2)
    }

    "foreachIndex2Combination" in {
      val array = Array(7, 8, 9)
      val buffer = new mutable.ArrayBuffer[(Int, Int)]
      array.foreachIndex2Combination((a, b) => buffer += ((a, b)))
      buffer.toList mustEqual List((0, 1), (0, 2), (1, 2))
    }
  }
}
