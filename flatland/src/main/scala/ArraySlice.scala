package flatland

import scala.collection.immutable

trait ArraySliceInt extends immutable.IndexedSeq[Int] {
  def length: Int

  @inline override def apply(idx: Int): Int

  @inline override def isEmpty = length == 0
  @inline override def slice(from: Int, until: Int): IndexedSeq[Int]

  // override protected[this] def newBuilder: mutable.Builder[Int, IndexedSeq[Int]] = new mutable.Builder[Int, IndexedSeq[Int]] {
  //   val self = new mutable.ArrayBuilder.ofInt
  //   override def +=(elem: Int): this.type = { self += elem; this }
  //   override def clear(): Unit = self.clear()
  //   override def result(): IndexedSeq[Int] = self.result()
  //   override def sizeHint(size: Int): Unit = self.sizeHint(size)
  // }
  @inline override def iterator: Iterator[Int]
  @inline def viewMapInt(f: Int => Int): ArraySliceInt
}

@inline final class ArraySliceIntValues(array: Array[Int], start: Int, val length: Int) extends ArraySliceInt {
  @inline override def apply(idx: Int): Int = array(start + idx)
  @inline override def slice(from: Int, until: Int): ArraySliceInt = new ArraySliceIntValues(array, start + from, until - from)
  @inline override def iterator: Iterator[Int] = array.iterator.slice(start, start + length)
  @inline def viewMapInt(f: Int => Int): ArraySliceInt = new ArraySliceIntMapped(array, start, length, f)
  // @inline override def update(idx: Int, elem: Int): Unit = array.update(start + idx, elem)
}

@inline final class ArraySliceIntMapped(array: Array[Int], start: Int, val length: Int, mapped: Int => Int) extends ArraySliceInt {
  @inline override def apply(idx: Int): Int = mapped(array(start + idx))
  @inline override def slice(from: Int, until: Int): ArraySliceInt = new ArraySliceIntMapped(array, start + from, until - from, mapped)
  @inline override def iterator: Iterator[Int] = array.iterator.slice(start, start + length).map(mapped)
  @inline def viewMapInt(f: Int => Int): ArraySliceInt = new ArraySliceIntMapped(array, start, length, i => f(mapped(i)))
}

object ArraySliceInt {
  @inline def empty = new ArraySliceIntValues(Array.empty, 0, 0)
  @inline def apply(array: Array[Int], start: Int, length: Int) = new ArraySliceIntValues(array, start, length)
  @inline def fromArray(array: Array[Int]): ArraySliceInt = apply(array, 0, array.length)
}
