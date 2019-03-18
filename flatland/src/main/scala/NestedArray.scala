package flatland

import flatland._

import scala.collection.mutable
import scala.reflect.ClassTag

// Simulates Array[Array[Int]] but stored only in one array for performance reasons.
// data: contains all data of the virtual nested arrays
// sliceArray: stores start/length of nested array interleaved

@inline final class NestedArrayInt(val data: Array[Int], val sliceArray: InterleavedArray[Int]) extends IndexedSeq[ArraySliceInt] {
  @inline def length: Int = sliceArray.elementCount
  @inline override def size: Int = length
  @inline override def isEmpty: Boolean = length == 0
  @inline override def nonEmpty: Boolean = length != 0

  @inline def sliceStart(idx: Int): Int = sliceArray.a(idx)
  @inline def sliceLength(idx: Int): Int = sliceArray.b(idx)
  @inline def sliceIsEmpty(idx: Int): Boolean = sliceLength(idx) == 0
  @inline def sliceNonEmpty(idx: Int): Boolean = sliceLength(idx) > 0
  @inline private def dataIndex(idx1: Int, idx2: Int): Int = sliceStart(idx1) + idx2

  @inline def anyContains(elem: Int) = data.contains(elem)

  @inline def apply(idx: Int): ArraySliceInt = new ArraySliceInt(data, sliceStart(idx), sliceLength(idx))
  @inline def safe(idx: Int): ArraySliceInt = {
    if (idx < 0 || length <= idx) new ArraySliceInt(data, 0, 0)
    else apply(idx)
  }
  @inline def apply(idx1: Int, idx2: Int): Int = data(dataIndex(idx1, idx2))
  @inline def get(idx1: Int, idx2: Int): Option[Int] = {
    if (0 <= idx1 && idx1 < length && 0 <= idx2 && idx2 < sliceLength(idx1))
      Some(apply(idx1, idx2))
    else None
  }
  @inline def update(idx1: Int, idx2: Int, newValue: Int): Unit = data(dataIndex(idx1, idx2)) = newValue
  @inline def foreachElement(idx: Int)(f: Int => Unit): Unit = {
    // fast iteration over sub-array without allocation
    var i = 0
    val n = sliceLength(idx)
    while (i < n) {
      f(apply(idx, i))
      i += 1
    }
  }
  @inline def foreachIndexAndElement(idx: Int)(f: (Int, Int) => Unit): Unit = {
    // fast iteration over sub-array without allocation
    var i = 0
    val n = sliceLength(idx)
    while (i < n) {
      f(i, apply(idx, i))
      i += 1
    }
  }
  @inline def foreachIndexAndSlice(f: (Int, ArraySliceInt) => Unit): Unit = {
    loop(length) { i =>
      f(i,apply(i))
    }
  }
  @inline def foreachSliceAndElement(idxArray: Array[Int])(f: Int => Unit): Unit = {
    idxArray.foreachElement { foreachElement(_)(f) } 
  }
  @inline def forall(idx: Int)(f: Int => Boolean): Boolean = {
    // fast iteration over sub-array without allocation
    var i = 0
    val n = sliceLength(idx)
    while (i < n) {
      if (!f(apply(idx, i))) return false
      i += 1
    }
    true
  }
  @inline def exists(idx: Int)(f: Int => Boolean): Boolean = {
    // fast iteration over sub-array without allocation
    var i = 0
    val n = sliceLength(idx)
    while (i < n) {
      if (f(apply(idx, i))) return true
      i += 1
    }
    false
  }
  @inline def contains(idx: Int)(elem: Int): Boolean = exists(idx)(_ == elem)

  @inline def collectFirst[T](idx: Int)(f: PartialFunction[Int, T]): Option[T] = {
    // fast iteration over sub-array without allocation
    var i = 0
    val n = sliceLength(idx)
    val safef: PartialFunction[Int, Unit] = f.andThen { t => return Some(t) }
    while (i < n) {
      safef.applyOrElse(apply(idx, i), (_: Int) => ())
      i += 1
    }
    None
  }

  @inline def count[T](idx: Int)(f: Int => Boolean): Int = {
    // fast iteration over sub-array without allocation
    var i = 0
    var counter = 0
    val n = sliceLength(idx)
    while (i < n) {
      if (f(apply(idx, i))) counter += 1
      i += 1
    }
    counter
  }

  @inline def flatMap[T](idx: Int)(f: Int => Array[T])(implicit classTag: ClassTag[T]): Array[T] = {
    val result = Array.newBuilder[T]
    // fast iteration over sub-array without allocation
    foreachElement(idx){ elem =>
      result ++= f(elem)
    }
    result.result
  }

  @inline def map[T](idx: Int)(f: Int => T)(implicit classTag: ClassTag[T]): Array[T] = {
    val n = sliceLength(idx)
    val result = new Array[T](n)
    // fast iteration over sub-array without allocation
    foreachIndexAndElement(idx){ (i, elem) =>
      result(i) = f(elem)
    }
    result
  }

  @inline def collect[T](idx: Int)(f: PartialFunction[Int, T])(implicit classTag: ClassTag[T]): Array[T] = {
    val result = Array.newBuilder[T]
    // fast iteration over sub-array without allocation
    val safef: PartialFunction[Int, Unit] = f.andThen { t => result += t }
    foreachIndexAndElement(idx){ (i, elem) =>
      safef.applyOrElse(elem, (_: Int) => ())
    }
    result.result
  }

  @inline def toArraySet(idx: Int):ArraySet = {
    val arraySet = ArraySet.create(this.length)
    foreachElement(idx)(arraySet.add)
    arraySet
  }

  def transposed:NestedArrayInt = {
    val counts = new Array[Int](length)
    data.foreachElement(counts(_) += 1)
    val builder = NestedArrayInt.builder(counts)
    loop(length) { i =>
      foreachElement(i) { builder.add(_, i) }
    }
    builder.result()
  }
}

final class NestedArrayIntBuilder(nestedArray: NestedArrayInt) {
  var filled = new Array[Int](nestedArray.length)
  def add(idx: Int, value: Int): Unit = {
    // assert(idx < nestedArray.length)
    // assert(filled(idx) < nestedArray.sliceLength(idx), idx)
    nestedArray.update(idx, filled(idx), value)
    filled(idx) += 1
  }

  def result(): NestedArrayInt = {
    filled = null
    nestedArray
  }
}

object NestedArrayInt {
  def apply(nested: Array[Array[Int]]): NestedArrayInt = {
    var currentStart = 0
    val sliceArray = InterleavedArray.create[Int](nested.length)
    nested.foreachIndexAndElement { (i, slice) =>
      sliceArray.updatea(i, currentStart)
      sliceArray.updateb(i, slice.length)

      currentStart += slice.length
    }

    val data = new Array[Int](currentStart)
    currentStart = 0
    nested.foreachIndexAndElement { (i, slice) =>
      slice.copyToArray(data, currentStart)
      currentStart += slice.length
    }

    new NestedArrayInt(data, sliceArray)
  }

  def apply(sliceLengths: Array[Int]): NestedArrayInt = {
    val sliceArray = InterleavedArray.create[Int](sliceLengths.length)
    var currentStart = 0
    sliceLengths.foreachIndexAndElement{ (i, sliceLength) =>
      sliceArray.updatea(i, currentStart)
      sliceArray.updateb(i, sliceLength)
      currentStart += sliceLength
    }
    val array = new Array[Int](currentStart)
    new NestedArrayInt(array, sliceArray)
  }

  @inline def builder(sliceLengths: Array[Int]): NestedArrayIntBuilder = {
    new NestedArrayIntBuilder(apply(sliceLengths)) //, sliceLengths)
  }

  def apply(nested: Array[mutable.ArrayBuilder.ofInt]): NestedArrayInt = {
    // ArrayBuilders can also be null to represent an empty builder
    var currentStart = 0
    val sliceArray = InterleavedArray.create[Int](nested.length)
    val builtSlices = new Array[Array[Int]](nested.length)

    nested.foreachIndexAndElement{ (i, sliceBuilder) =>
      if (sliceBuilder != null) {
        val slice = sliceBuilder.result()
        builtSlices(i) = slice
        sliceArray.updatea(i, currentStart)
        sliceArray.updateb(i, slice.length)

        currentStart += slice.length
      } else {
        // empty builder => empty slice
        builtSlices(i) = null
        sliceArray.updatea(i, currentStart)
        sliceArray.updateb(i, 0)
      }
    }

    val array = new Array[Int](currentStart)
    currentStart = 0
    builtSlices.foreachIndexAndElement{ (i, slice) =>
      if (slice != null) {
        slice.copyToArray(array, currentStart)
        currentStart += slice.length
      }
    }

    new NestedArrayInt(array, sliceArray)
  }
}
