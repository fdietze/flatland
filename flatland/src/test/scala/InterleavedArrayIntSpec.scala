package flatland.test

import flatland._

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class InterleavedArrayIntSpec extends AnyFreeSpec with Matchers {

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

    "one interleaved tuple (x,y) - update at once" in {
      val interleaved = InterleavedArrayInt.create(1)
      interleaved.update(0, 5, 7)

      interleaved.x(0) mustEqual 5
      interleaved.y(0) mustEqual 7
    }

    "elementCount" in {
      val interleaved = InterleavedArrayInt.create(17)
      interleaved.elementCount mustEqual 17
    }

    "exists" in {
      val interleaved = InterleavedArrayInt(1 -> 3, 4 -> 4, 2 -> 5)
      interleaved.exists(_ == _) mustEqual true
      interleaved.exists(_ > _) mustEqual false
    }

    "toIndexdSeq" in {
      val interleaved = InterleavedArrayInt.create(2)
      interleaved.updatex(0, 5)
      interleaved.updatey(0, 7)
      interleaved.updatex(1, 3)
      interleaved.updatey(1, 4)

      interleaved.toList mustEqual List((5, 7), (3, 4))
    }

    "from IndexedSeq[(Int,Int)]" in {
      val interleaved = InterleavedArrayInt(3 -> 4, 7 -> 9)
      interleaved.toList mustEqual List((3, 4), (7, 9))
    }

    "builder" in {
      val builder = InterleavedArrayInt.builder()
      builder.add(5, 7)
      builder.add(3, 4)

      val interleaved = builder.result()
      interleaved.toList mustEqual List((5, 7), (3, 4))
    }
  }
}
