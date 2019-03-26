package flatland

import scala.reflect.ClassTag

object InterleavedArrayDouble {
  @inline def create(n: Int): InterleavedArrayDouble = new InterleavedArrayDouble(new Array[Double](n * 2))

  implicit def toIndexdSeq(interleaved: InterleavedArrayDouble): IndexedSeq[(Double, Double)] = new IndexedSeq[(Double, Double)] {
    def apply(idx: Int): (Double, Double) = (interleaved.a(idx), interleaved.b(idx))
    def length: Int = interleaved.elementCount
  }
}

@inline final class InterleavedArrayDouble(val interleaved: Array[Double]) {
  @inline def a(i: Int): Double = interleaved(i * 2)
  @inline def b(i: Int): Double = interleaved(i * 2 + 1)
  @inline def updatea(i: Int, value: Double): Unit = interleaved(i * 2) = value
  @inline def updateb(i: Int, value: Double): Unit = interleaved(i * 2 + 1) = value

  @inline def x(i: Int): Double = a(i)
  @inline def y(i: Int): Double = b(i)
  @inline def updatex(i: Int, value: Double): Unit = updatea(i, value)
  @inline def updatey(i: Int, value: Double): Unit = updateb(i, value)

  @inline def left(i: Int): Double = a(i)
  @inline def right(i: Int): Double = b(i)
  @inline def updateLeft(i: Int, value: Double): Unit = updatea(i, value)
  @inline def updateRight(i: Int, value: Double): Unit = updateb(i, value)

  @inline def elementCount: Int = interleaved.length / 2

  @inline def foreachTwoElements(f: (Double, Double) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while (i < n) {
      f(a(i), b(i))
      i += 1
    }
  }
  @inline def foreachIndexAndTwoElements(f: (Int, Double, Double) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while (i < n) {
      f(i, a(i), b(i))
      i += 1
    }
  }
}
