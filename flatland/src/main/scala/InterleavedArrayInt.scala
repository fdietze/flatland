package flatland

import scala.reflect.ClassTag
import collection.immutable
import collection.mutable

@inline final class InterleavedArrayInt(val interleaved: Array[Long]) {
  @inline private def extractHi(l: Long): Int             = (l >> 32).toInt
  @inline private def extractLo(l: Long): Int             = l.toInt
  @inline private def combineToLong(a: Int, b: Int): Long = ((a.toLong) << 32) | (b & 0xffffffffL)

  @inline def a(i: Int): Int                       = extractHi(interleaved(i))
  @inline def b(i: Int): Int                       = extractLo(interleaved(i))
  @inline def update(i: Int, a: Int, b: Int): Unit = { interleaved(i) = combineToLong(a, b) }
  @inline def updatea(i: Int, value: Int): Unit    = update(i, value, b(i))
  @inline def updateb(i: Int, value: Int): Unit    = update(i, a(i), value)

  @inline def x(i: Int): Int                    = a(i)
  @inline def y(i: Int): Int                    = b(i)
  @inline def updatex(i: Int, value: Int): Unit = updatea(i, value)
  @inline def updatey(i: Int, value: Int): Unit = updateb(i, value)

  @inline def left(i: Int): Int                     = a(i)
  @inline def right(i: Int): Int                    = b(i)
  @inline def updateLeft(i: Int, value: Int): Unit  = updatea(i, value)
  @inline def updateRight(i: Int, value: Int): Unit = updateb(i, value)

  @inline def elementCount: Int = interleaved.length
  @inline def length: Int       = elementCount
  @inline def size: Int         = elementCount
  @inline def isEmpty           = elementCount == 0
  @inline def nonEmpty          = elementCount > 0

  @inline def foreachTwoElements(f: (Int, Int) => Unit): Unit              = {
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

  def exists(f: (Int, Int) => Boolean): Boolean = {
    flatland.exists(elementCount)(i => f(a(i), b(i)))
  }
}

object InterleavedArrayInt {
  @inline def create(n: Int): InterleavedArrayInt = new InterleavedArrayInt(new Array[Long](n))

  @inline def apply(tuples: (Int, Int)*): InterleavedArrayInt            = apply(tuples.toIndexedSeq)
  @inline def apply(tuples: IndexedSeq[(Int, Int)]): InterleavedArrayInt = {
    val interleaved = create(tuples.length)
    tuples.foreachIndexAndElement { (i, tuple) =>
      interleaved.updatea(i, tuple._1)
      interleaved.updateb(i, tuple._2)
    }
    interleaved
  }

  @inline def empty = create(0)

  implicit def toIndexdSeq(interleaved: InterleavedArrayInt): IndexedSeq[(Int, Int)] = new IndexedSeq[(Int, Int)] {
    def apply(idx: Int): (Int, Int) = (interleaved.a(idx), interleaved.b(idx))
    def length: Int                 = interleaved.elementCount
  }

  def builder() = new InterleavedArrayIntBuilder
}

@inline final class InterleavedArrayIntBuilder {
  @inline private def extractHi(l: Long): Int             = (l >> 32).toInt
  @inline private def extractLo(l: Long): Int             = l.toInt
  @inline private def combineToLong(a: Int, b: Int): Long = ((a.toLong) << 32) | (b & 0xffffffffL)

  val self = new mutable.ArrayBuilder.ofLong

  @inline def add(a: Int, b: Int): Unit = {
    self += combineToLong(a, b)
  }

  @inline def result() = new InterleavedArrayInt(self.result())
}
