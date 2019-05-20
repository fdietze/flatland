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

  "flatMap with index" - {
    "empty" in {
      val buffer = Array.empty[Int]
      buffer.flatMapWithIndex[Long]((idx, i) => ???) mustEqual Array.empty[Long]
    }
    "array" in {
      val buffer = Array("a", "b", "meh", "c")
      buffer.flatMapWithIndex[String]((idx, str) => if (str.length ==1) Array(idx + str) else Array.empty) mustEqual Array("0a", "1b", "3c")
    }
  }
}
