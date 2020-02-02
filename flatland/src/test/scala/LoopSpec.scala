package flatland.test

import flatland._

import org.scalatest._
import collection.mutable
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class LoopSpec extends AnyFreeSpec with Matchers {

  "Simple Loop" - {
    val buffer = new mutable.ArrayBuffer[Int]
    loop(3)(buffer += _)
    buffer.toList mustEqual List(0, 1, 2)
    ()
  }

  "Loop with start" - {
    val buffer = new mutable.ArrayBuffer[Int]
    loop(3, start = 1)(buffer += _)
    buffer.toList mustEqual List(1, 2)
    ()
  }
}
