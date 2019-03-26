package flatland.test

import flatland._

import org.scalatest._

class InterleavedArrayIntSpec extends FreeSpec with MustMatchers {

  "InterleavedArrayInt" - {
    "one interleaved tuple (a,b)" in {
      val interleaved = InterleavedArrayInt.create(1)
      interleaved.updatea(0, 5)
      interleaved.updateb(0, 7)

      interleaved.a(0) mustEqual 5
      interleaved.b(0) mustEqual 7
    }

    "one interleaved tuple (left,right)" in {
      val interleaved = InterleavedArrayInt.create(1)
      interleaved.updateLeft(0, 5)
      interleaved.updateRight(0, 7)

      interleaved.left(0) mustEqual 5
      interleaved.right(0) mustEqual 7
    }

    "one interleaved tuple (x,y)" in {
      val interleaved = InterleavedArrayInt.create(1)
      interleaved.updatex(0, 5)
      interleaved.updatey(0, 7)

      interleaved.x(0) mustEqual 5
      interleaved.y(0) mustEqual 7
    }

    "elementCount" in {
      val interleaved = InterleavedArrayInt.create(17)
      interleaved.elementCount mustEqual 17
    }

    "toIndexdSeq" in {
      val interleaved = InterleavedArrayInt.create(2)
      interleaved.updatex(0, 5)
      interleaved.updatey(0, 7)
      interleaved.updatex(1, 3)
      interleaved.updatey(1, 4)

      interleaved.toList mustEqual List((5,7), (3,4))
    }
  }
}
