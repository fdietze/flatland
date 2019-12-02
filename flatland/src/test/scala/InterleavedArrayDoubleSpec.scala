package flatland.test

import flatland._

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class InterleavedArrayDoubleSpec extends AnyFreeSpec with Matchers {

  "InterleavedArrayDouble" - {
    "one interleaved tuple (a,b)" in {
      val interleaved = InterleavedArrayDouble.create(1)
      interleaved.updatea(0, 5.0)
      interleaved.updateb(0, 7.0)

      interleaved.a(0) mustEqual 5.0
      interleaved.b(0) mustEqual 7.0
    }

    "one interleaved tuple (left,right)" in {
      val interleaved = InterleavedArrayDouble.create(1)
      interleaved.updateLeft(0, 5.0)
      interleaved.updateRight(0, 7.0)

      interleaved.left(0) mustEqual 5.0
      interleaved.right(0) mustEqual 7.0
    }

    "one interleaved tuple (x,y)" in {
      val interleaved = InterleavedArrayDouble.create(1)
      interleaved.updatex(0, 5)
      interleaved.updatey(0, 7)

      interleaved.x(0) mustEqual 5.0
      interleaved.y(0) mustEqual 7.0
    }

    "one interleaved tuple (x,y) - update at once" in {
      val interleaved = InterleavedArrayDouble.create(1)
      interleaved.update(0, 5.0, 7.0)

      interleaved.x(0) mustEqual 5.0
      interleaved.y(0) mustEqual 7.0
    }

    "elementCount" in {
      val interleaved = InterleavedArrayDouble.create(17)
      interleaved.elementCount mustEqual 17
    }

    "toIndexdSeq" in {
      val interleaved = InterleavedArrayDouble.create(2)
      interleaved.updatex(0, 5.0)
      interleaved.updatey(0, 7.0)
      interleaved.updatex(1, 3.0)
      interleaved.updatey(1, 4.0)

      interleaved.toList mustEqual List((5.0, 7.0), (3.0, 4.0))
    }
  }
}
