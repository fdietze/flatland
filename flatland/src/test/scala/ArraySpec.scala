package flatland.test

import flatland._

import org.scalatest._

class ArraySpec extends FreeSpec with MustMatchers {

  "map with index" - {
    "empty" in {
      val buffer = Array.empty[Int]
      buffer.mapWithIndex[Long]((idx, i) => ???) mustEqual Array.empty[Long]
    }
    "array" in {
      val buffer = Array("a", "b", "c")
      buffer.mapWithIndex[String](_ + _) mustEqual Array("0a", "1b", "2c")
    }
  }
}
