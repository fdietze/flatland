package flatland.test

import flatland._

import org.scalatest._
import collection.mutable

class LoopSpec extends FreeSpec with MustMatchers {

  "Simple Loop" - {
    val buffer = new mutable.ArrayBuffer[Int]
    loop(3)(buffer += _)
    buffer.toList mustEqual List(0,1,2)
  }

  "Loop with start" - {
    val buffer = new mutable.ArrayBuffer[Int]
    loop(3, start = 1)(buffer += _)
    buffer.toList mustEqual List(1,2)
  }
}
