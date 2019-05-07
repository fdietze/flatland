package flatland

import scala.reflect.ClassTag

object InterleavedArrayInt {
  @inline def create(n: Int): InterleavedArrayInt = new InterleavedArrayInt(new Array[Int](n * 2))

  @inline def apply(tuples: (Int,Int)*):InterleavedArrayInt = apply(tuples.toArray)
  @inline def apply(tuples: IndexedSeq[(Int, Int)]): InterleavedArrayInt = {
    val interleaved = create(tuples.length)
    tuples.foreachIndexAndElement{ (i, tuple) =>
      interleaved.updatea(i, tuple._1)
      interleaved.updateb(i, tuple._2)
    }
    interleaved
  }

  @inline def empty = create(0)

  implicit def toIndexdSeq(interleaved: InterleavedArrayInt): IndexedSeq[(Int, Int)] = new IndexedSeq[(Int, Int)] {
    def apply(idx: Int): (Int, Int) = (interleaved.a(idx), interleaved.b(idx))
    def length: Int = interleaved.elementCount
  }
}

@inline final class InterleavedArrayInt(val interleaved: Array[Int]) {
  @inline def a(i: Int): Int = interleaved(i * 2)
  @inline def b(i: Int): Int = interleaved(i * 2 + 1)
  @inline def updatea(i: Int, value: Int): Unit = interleaved(i * 2) = value
  @inline def updateb(i: Int, value: Int): Unit = interleaved(i * 2 + 1) = value
  @inline def update(i: Int, a: Int, b: Int): Unit = { updatea(i, a); updateb(i, b) }

  @inline def x(i: Int): Int = a(i)
  @inline def y(i: Int): Int = b(i)
  @inline def updatex(i: Int, value: Int): Unit = updatea(i, value)
  @inline def updatey(i: Int, value: Int): Unit = updateb(i, value)

  @inline def left(i: Int): Int = a(i)
  @inline def right(i: Int): Int = b(i)
  @inline def updateLeft(i: Int, value: Int): Unit = updatea(i, value)
  @inline def updateRight(i: Int, value: Int): Unit = updateb(i, value)

  @inline def elementCount: Int = interleaved.length / 2

  @inline def foreachTwoElements(f: (Int, Int) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while (i < n) {
      f(a(i), b(i))
      i += 1
    }
  }
  @inline def foreachIndexAndTwoElements(f: (Int, Int, Int) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while (i < n) {
      f(i, a(i), b(i))
      i += 1
    }
  }
}
