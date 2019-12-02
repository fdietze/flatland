package flatland.test

import flatland._

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ArrayStackSpec extends AnyFreeSpec with Matchers {

  "ArrayStackInt" - {
    "fill to full capacity" in {
      val stack = ArrayStackInt.create(2)
      stack.size mustEqual 0
      stack.isEmpty mustEqual true
      stack.isFull mustEqual false

      stack.push(3)
      stack.size mustEqual 1
      stack.isEmpty mustEqual false
      stack.isFull mustEqual false

      stack.push(7)
      stack.size mustEqual 2
      stack.isEmpty mustEqual false
      stack.isFull mustEqual true

      stack.pop() mustEqual 7
      stack.size mustEqual 1
      stack.isEmpty mustEqual false
      stack.isFull mustEqual false

      stack.pop() mustEqual 3
      stack.size mustEqual 0
      stack.isEmpty mustEqual true
      stack.isFull mustEqual false
    }
  }
}
