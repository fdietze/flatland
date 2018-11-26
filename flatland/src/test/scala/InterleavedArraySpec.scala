package flatland.test

import flatland._

import org.scalatest._

class InterleavedArraySpec extends FreeSpec with MustMatchers {

  "InterleavedArray" - {
    "one interleaved tuple (a,b)" in {
      val interleaved = InterleavedArray.create[Int](1)
      interleaved.updatea(0,5)
      interleaved.updateb(0,7)

      interleaved.a(0) mustEqual 5
      interleaved.b(0) mustEqual 7
    }

    "one interleaved tuple (x,y)" in {
      val interleaved = InterleavedArray.create[Int](1)
      interleaved.updatex(0,5)
      interleaved.updatey(0,7)

      interleaved.x(0) mustEqual 5
      interleaved.y(0) mustEqual 7
    }

    "elementCount" in {
      val interleaved = InterleavedArray.create[Int](17)
      interleaved.elementCount mustEqual 17
    }
  }
}
