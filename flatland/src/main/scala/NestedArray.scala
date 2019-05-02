package flatland

import scala.collection.mutable
import scala.reflect.ClassTag

// Simulates Array[Array[Int]] but stored only in one array for performance reasons.
// data = (array of start positions)[length, sub-array elements](number of sub-arrays)
//
// Many algorithms assume that the sub-arrays store indices into the outer array.
// like depthFirstSearch()

@inline final class NestedArrayInt(val data: Array[Int]) extends IndexedSeq[ArraySliceInt] {
  @inline def length: Int = data(data.length - 1)
  @inline override def size: Int = length
  @inline override def isEmpty: Boolean = length == 0
  @inline override def nonEmpty: Boolean = length != 0

  @inline def sliceStart(idx: Int): Int = data(idx)
  @inline def sliceLength(idx: Int): Int = data(sliceStart(idx))
  @inline def sliceIsEmpty(idx: Int): Boolean = sliceLength(idx) == 0
  @inline def sliceNonEmpty(idx: Int): Boolean = sliceLength(idx) > 0
  @inline private def dataIndex(idx1: Int, idx2: Int): Int = sliceStart(idx1) + idx2 + 1

  @inline def apply(idx: Int): ArraySliceInt = new ArraySliceInt(data, sliceStart(idx) + 1, sliceLength(idx))
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
  @inline def foreachIndex(idx: Int)(f: (Int) => Unit): Unit = {
    loop(sliceLength(idx))(i => f(i))
  }
  @inline def foreachElement(idx: Int)(f: Int => Unit): Unit = {
    foreachIndex(idx) { i =>
      f(apply(idx, i))
    }
  }
  @inline def foreachIndexAndElement(idx: Int)(f: (Int, Int) => Unit): Unit = {
    foreachIndex(idx) { i =>
      f(i, apply(idx, i))
    }
  }
  @inline def foreachIndexAndSlice(f: (Int, ArraySliceInt) => Unit): Unit = {
    loop(length) { i =>
      f(i, apply(i))
    }
  }
  @inline def foreachSliceAndElement(idxArray: Array[Int])(f: Int => Unit): Unit = {
    idxArray.foreachElement { foreachElement(_)(f) }
  }
  @inline def forall(idx: Int)(f: Int => Boolean): Boolean = {
    var i = 0
    val n = sliceLength(idx)
    var all = true
    while (all && i < n) {
      if (!f(apply(idx, i))) all = false
      i += 1
    }
    all
  }

  @inline def exists(idx: Int)(f: Int => Boolean): Boolean = {
    var i = 0
    val n = sliceLength(idx)
    var notExists = true
    while (notExists && i < n) {
      if (f(apply(idx, i))) notExists = false
      i += 1
    }
    !notExists
  }

  @inline def find(idx: Int)(f: Int => Boolean): Option[Int] = {
    var i = 0
    val n = sliceLength(idx)
    var result: Option[Int] = None
    while (result.isEmpty && i < n) {
      val iValue = apply(idx, i)
      if (f(iValue)) result = Some(iValue)
      i += 1
    }

    result
  }

  @inline def contains(idx: Int)(elem: Int): Boolean = {
    exists(idx)(_ == elem)
  }

  @inline def anyContains(elem: Int): Boolean = {
    val n = length
    var notFound = true

    var idx = 0
    var i = 0
    while (notFound && idx < n) {
      val sliceLen = sliceLength(idx)
      i = 0
      while (notFound && i < sliceLen) {
        if (apply(idx, i) == elem) notFound = false
        i += 1
      }

      idx += 1
    }

    !notFound
  }

  @inline def collectFirst[T](idx: Int)(f: PartialFunction[Int, T]): Option[T] = {
    var i = 0
    val n = sliceLength(idx)
    val safef: PartialFunction[Int, Unit] = f.andThen { t => return Some(t) }
    while (i < n) {
      safef.applyOrElse(apply(idx, i), (_: Int) => ())
      i += 1
    }
    None
  }

  @inline def count(idx: Int)(f: Int => Boolean): Int = {
    var counter = 0
    foreachElement(idx) { elem =>
      if (f(elem)) counter += 1
    }
    counter
  }

  @inline def foldLeft[T](idx: Int)(neutral: T)(f: (T, Int) => T): T = {
    var agg: T = neutral
    foreachElement(idx) { elem =>
      agg = f(agg, elem)
    }
    agg
  }

  @inline def minByInt(idx: Int)(initMin: Int)(f: Int => Int): Int = {
    var min = initMin
    foreachElement(idx) { elem =>
      val current = f(elem)
      if (current < min) min = current
    }
    min
  }

  @inline def maxByInt(idx: Int)(initMax: Int)(f: Int => Int): Int = {
    var max = initMax
    foreachElement(idx) { elem =>
      val current = f(elem)
      if (current > max) max = current
    }
    max
  }

  @inline def flatMap[T](idx: Int)(f: Int => Array[T])(implicit classTag: ClassTag[T]): Array[T] = {
    val result = Array.newBuilder[T]
    foreachElement(idx){ elem =>
      result ++= f(elem)
    }
    result.result
  }

  @inline def map[T](idx: Int)(f: Int => T)(implicit classTag: ClassTag[T]): Array[T] = {
    val n = sliceLength(idx)
    val result = new Array[T](n)
    foreachIndexAndElement(idx){ (i, elem) =>
      result(i) = f(elem)
    }
    result
  }

  @inline def collect[T](idx: Int)(f: PartialFunction[Int, T])(implicit classTag: ClassTag[T]): Array[T] = {
    val result = Array.newBuilder[T]
    val safef: PartialFunction[Int, Unit] = f.andThen { t => result += t }
    foreachIndexAndElement(idx){ (i, elem) =>
      safef.applyOrElse(elem, (_: Int) => ())
    }
    result.result
  }

  @inline def toArraySet(idx: Int): ArraySet = {
    val arraySet = ArraySet.create(this.length)
    foreachElement(idx)(arraySet.add)
    arraySet
  }

  def transposed: NestedArrayInt = {
    val counts = new Array[Int](length)
    loop(length){ idx =>
      loop(sliceLength(idx)) { i =>
        counts(apply(idx, i)) += 1
      }
    }
    val builder = NestedArrayInt.builder(counts)
    loop(length) { idx =>
      foreachElement(idx) { i =>
        builder.add(i, idx)
      }
    }
    builder.result()
  }

  def depthFirstSearchToArray(start: Int): Array[Int] = {
    val builder = new mutable.ArrayBuilder.ofInt
    depthFirstSearchGeneric(
      init = (enqueue, _) => enqueue(start),
      processVertex = builder += _
    )
    builder.result()
  }


  // inlining workarounds:
  // (https://github.com/scala-js/scala-js/issues/3624)
  @inline private def loopConditionGuardDefault: (() => Boolean) => Boolean =
    condition => condition()
  @inline private def advanceGuardDefault[PROCESSRESULT]: (PROCESSRESULT, () => Unit) => Unit =
    (result: PROCESSRESULT, advance: () => Unit) => advance()
  @inline private def enqueueGuardDefault: (Int, () => Unit) => Unit =
    (elem, enqueue) => enqueue()

  // inline is important for inlining the lambda parameters
  @inline def depthFirstSearchGeneric[PROCESSRESULT](
    init: (Int => Unit, ArrayStackInt) => Unit,
    processVertex: Int => PROCESSRESULT,
    loopConditionGuard: (() => Boolean) => Boolean = loopConditionGuardDefault,
    advanceGuard: (PROCESSRESULT, () => Unit) => Unit = advanceGuardDefault,
    enqueueGuard1: (Int, () => Unit) => Unit = enqueueGuardDefault,
    enqueueGuard2: (Int, () => Unit) => Unit = enqueueGuardDefault
  ): Unit = {
    flatland.depthFirstSearchGeneric(
      vertexCount = length,
      foreachSuccessor = (idx, f) => foreachElement(idx)(f),
      init,
      processVertex,
      loopConditionGuard,
      advanceGuard,
      enqueueGuard1 = enqueueGuard1,
      enqueueGuard2 = enqueueGuard2
    )
  }
}

@inline final class NestedArrayIntBuilder(nestedArray: NestedArrayInt) {
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
    val n = nested.length
    var currentStart = n
    nested.foreachElement { slice =>
      currentStart += (slice.length + 1)
    }

    val dataLength = currentStart + 1 // 1 is for number of nested arrays
    val data = new Array[Int](dataLength)
    currentStart = n
    nested.foreachIndexAndElement { (i, slice) =>
      val start = currentStart
      val sliceLength = slice.length
      data(i) = start
      data(start) = sliceLength
      slice.copyToArray(data, start + 1)
      currentStart += (sliceLength + 1)
    }
    data(dataLength - 1) = n

    new NestedArrayInt(data)
  }

  @inline def empty = new NestedArrayInt(data = Array(0))

  def apply(nested: Array[mutable.ArrayBuilder.ofInt]): NestedArrayInt = {
    // ArrayBuilders can also be null to represent an empty builder
    val n = nested.length
    var currentStart = n
    val arrays = new Array[Array[Int]](n)
    nested.foreachIndexAndElement { (i, slice) =>
      val array = if (slice == null) null else slice.result()
      if (slice != null) slice.clear() //TODO remove!
      arrays(i) = array
      currentStart += (if (array == null) 1 else (array.length + 1))
    }

    val dataLength = currentStart + 1 // 1 is for number of nested arrays
    val data = new Array[Int](dataLength)
    currentStart = n
    nested.foreachIndex { i =>
      val slice = arrays(i)
      val sliceLength = if (slice == null) 0 else slice.length
      val start = currentStart
      data(i) = start
      data(start) = sliceLength
      if (slice != null) slice.copyToArray(data, start + 1)
      currentStart += (sliceLength + 1)
    }
    data(dataLength - 1) = n

    new NestedArrayInt(data)
  }

  def apply(sliceLengths: Array[Int]): NestedArrayInt = {
    val n = sliceLengths.length
    var currentStart = n
    sliceLengths.foreachElement { sliceLength =>
      currentStart += (sliceLength + 1)
    }

    val dataLength = currentStart + 1 // 1 is for number of nested arrays
    val data = new Array[Int](dataLength)
    currentStart = n

    sliceLengths.foreachIndexAndElement { (i, sliceLength) =>
      val start = currentStart
      data(i) = start
      data(start) = sliceLength
      currentStart += (sliceLength + 1)
    }
    data(dataLength - 1) = n

    new NestedArrayInt(data)
  }

  @inline def builder(sliceLengths: Array[Int]): NestedArrayIntBuilder = {
    new NestedArrayIntBuilder(apply(sliceLengths)) //, sliceLengths)
  }
}
