package flatland

import scala.collection.mutable
import scala.reflect.ClassTag

// Simulates Array[Array[Int]] but stored only in one flat array for performance reasons.
// data = n*sliceStart_i, n*(length_i, slice_i), n
// note: the start of the first slice equals the number of sub-arrays,
// since the slices start after n positions.
// But we still need the last value for the empty-case.
//
// Many algorithms assume that the sub-arrays store indices into the outer array.
// like
// - depthFirstSearch
// - transposed

@inline final class NestedArrayInt(val data: Array[Int]) extends IndexedSeq[ArraySliceInt] {
  @inline def length: Int = data(0) // == data(data.length - 1)
  @inline override def size: Int = length
  @inline override def isEmpty: Boolean = length == 0
  @inline override def nonEmpty: Boolean = length != 0

  @inline def sliceDataStart(idx: Int): Int = data(idx)
  @inline def sliceStart(idx: Int): Int = sliceDataStart(idx) + 1
  @inline def sliceLength(idx: Int): Int = data(sliceDataStart(idx))
  @inline def sliceEnd(idx: Int): Int = sliceStart(idx) + sliceLength(idx)
  @inline def sliceIsEmpty(idx: Int): Boolean = sliceLength(idx) == 0
  @inline def sliceNonEmpty(idx: Int): Boolean = sliceLength(idx) > 0
  @inline private def dataIndex(idx1: Int, idx2: Int): Int = sliceDataStart(idx1) + idx2 + 1

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

  @inline def foreachIndex(idx: Int)(f: (Int) => Unit): Unit = {
    loop(sliceLength(idx))(i => f(i))
  }

  @inline def foreachIndex(f: (Int) => Unit): Unit = {
    loop(length)(i => f(i))
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
    flatland.forall(sliceLength(idx))(i => f(apply(idx, i)))
  }

  @inline def exists(idx: Int)(f: Int => Boolean): Boolean = {
    flatland.exists(sliceLength(idx)) { i => f(apply(idx, i)) }
  }

  @inline def find(idx: Int)(f: Int => Boolean): Option[Int] = {
    var i = 0
    val n = sliceLength(idx)
    var result: Option[Int] = None
    var notFound = true
    while (notFound && i < n) {
      val elem = apply(idx, i)
      if (f(elem)) {
        notFound = false
        result = Some(elem)
      }
      i += 1
    }

    result
  }

  @inline def indexOf(idx: Int)(elem: Int): Int = {
    var i = 0
    val n = sliceLength(idx)
    var index = -1
    while (index == -1 && i < n) {
      if (apply(idx, i) == elem) {
        index = i
      }
      i += 1
    }

    index
  }

  @inline def contains(idx: Int)(elem: Int): Boolean = {
    exists(idx)(_ == elem)
  }

  @inline def whileElement(idx: Int)(f: Int => Boolean): Unit = {
    var i = 0
    val n = sliceLength(idx)
    var notFound = true
    while (notFound && i < n) {
      if (!f(apply(idx, i))) notFound = false
      i += 1
    }
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
        notFound = apply(idx, i) != elem
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

  @inline def flatMap[T: ClassTag](idx: Int)(f: Int => Array[T]): Array[T] = {
    val result = Array.newBuilder[T]
    foreachElement(idx){ elem =>
      result ++= f(elem)
    }
    result.result
  }

  @inline def map[T: ClassTag](idx: Int)(f: Int => T): Array[T] = {
    val n = sliceLength(idx)
    val result = new Array[T](n)
    foreachIndexAndElement(idx){ (i, elem) =>
      result(i) = f(elem)
    }
    result
  }

  @inline def collect[T: ClassTag](idx: Int)(f: PartialFunction[Int, T]): Array[T] = {
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
      init = (stack, _) => stack.push(start),
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
    init: (ArrayStackInt, ArraySet) => Unit,
    processVertex: Int => PROCESSRESULT,
    loopConditionGuard: (() => Boolean) => Boolean = loopConditionGuardDefault,
    advanceGuard: (PROCESSRESULT, () => Unit) => Unit = advanceGuardDefault,
    enqueueGuard: (Int, () => Unit) => Unit = enqueueGuardDefault,
  ): Unit = {
    flatland.depthFirstSearchGeneric(
      vertexCount = length,
      foreachSuccessor = (idx, f) => foreachElement(idx)(f),
      init,
      processVertex,
      loopConditionGuard,
      advanceGuard,
      enqueueGuard = enqueueGuard,
    )
  }

  def changedWithAssertions(
    addIdx: Int = 0,
    addElem: InterleavedArrayInt = InterleavedArrayInt.empty, // Array[idx -> elem]
    delElem: InterleavedArrayInt = InterleavedArrayInt.empty // Array[idx -> position]
  ): NestedArrayInt = {
    assert(addElem.forall{ case (idx, elem) => idx < (length + addIdx) }, "addElem: invalid index")
    assert(addElem.sortBy(_._1).sameElements(addElem), "addElem not sorted by idx")

    assert(delElem.map(_._1).sorted.sameElements(delElem.map(_._1)), s"delElem: not sorted by idx: ${delElem.toList}")
    assert(delElem.groupBy(_._1).forall( grouped => grouped._2.sorted.sameElements(grouped._2)), s"delElem: positions not sorted: ${delElem.groupBy(_._1)}")
    assert(delElem.groupBy(_._1).forall( grouped => grouped._2.distinct.size == grouped._2.size), s"delElem: positions contains duplicates: ${delElem.groupBy(_._1)}")
    assert(delElem.forall{case (idx, pos) => pos < sliceLength(idx)}, s"delElem: positon out of bounds: ${delElem.toList}")

    changed(addIdx, addElem, delElem)
  }

  def changed(
    addIdx: Int = 0,
    addElem: InterleavedArrayInt = InterleavedArrayInt.empty, // Array[idx -> elem]
    delElem: InterleavedArrayInt = InterleavedArrayInt.empty // Array[idx -> position]
  ): NestedArrayInt = {
    // IMPORTANT:
    // For the operations to be efficient, changed() assumes all assertions from changedSafe.

    // def debug(arr: Array[Int]) = {
      // val length = arr(arr.length - 1)
      // print(arr.take(length).map(x => f"$x%03d".replace("9999999", "___")).mkString("  ") + "| ")
      // print(arr.drop(length).dropRight(1).map(x => f"$x%03d".replace("9999999", "___")).mkString("  "))
      // println(f" |$length%03d")
    // }

    @inline def prev = this
    // println(this.toVector.map(_.toVector))
    // debug(prev.data)
    // println(s"---addIdx: ${addIdx.toList}, addElem: ${addElem.toList}, delElem: ${delElem.toList}")
    val nextDataSize = prev.data.length + (addIdx * 2) + addElem.length - delElem.length
    // println(Array.tabulate(data.length max nextDataSize)(i => f"$i%03d").mkString(".."))
    // println("-" * 80)
    val nextData = new Array[Int](nextDataSize)
    // val nextData = new {
    //   // FOR DEBUGGING ONLY
    //   val arr = Array.fill(nextDataSize)(9999999)
    //   var reads = 0
    //   var writes = 0
    //   def length = arr.length
    //   def apply(i:Int) = {reads += 1;arr(i)}
    //   val written = mutable.HashSet.empty[Int]
    //   def update(i:Int, elem:Int) = {
    //     assert(!written.contains(i), s"writing to the same position twice: $i")
    //     written += i
    //     writes += 1
    //     assert(i < length, s"write out of bounds: $i")
    //     assert(writes <= length, s"more writes than necessary: $writes")
    //     arr.update(i,elem)
    //   }
    // }
    val next = new NestedArrayInt(nextData)

    // set the number of slices
    val sliceCount = prev.length + addIdx
    nextData(nextData.length - 1) = sliceCount
    // println("set number of slices")
    // debug(nextData.arr)

    // println("recalculate slice starts and lengths (using addElem, delElem)")
    val addElemLength = addElem.length
    val delElemLength = delElem.length
    var addElemIdx = 0
    var delElemIdx = 0
    var sliceDataStartPos = prev.length + addIdx
    loop(prev.length) { idx =>
      nextData(idx) = sliceDataStartPos
      var sliceLength = prev.sliceLength(idx)

      // println(s"\n  idx: $idx (addElemIdx: $addElemIdx, delElemIdx: $delElemIdx)")
      var copySourcePos = prev.sliceStart(idx)
      var copyTargetPos = sliceDataStartPos + 1 // pos after the length field
      while(delElemIdx < delElemLength && delElem.a(delElemIdx) == idx) {
        val delPos = prev.sliceStart(idx) + delElem.b(delElemIdx)
        // println(s"    copy elements $copySourcePos -> $copyTargetPos until next deleted position $delPos")
        while(copySourcePos < delPos) {
          // println(s"    copyTargetPos: $copyTargetPos value: ${prev.data(copySourcePos)}")
          nextData(copyTargetPos) = prev.data(copySourcePos)
          copyTargetPos += 1
          copySourcePos += 1
        }
        copySourcePos += 1 // skip deleted element
        // debug(nextData.arr)


        sliceLength -= 1
        delElemIdx += 1
        // println(s"  idx: $idx (addElemIdx: $addElemIdx, delElemIdx: $delElemIdx)")
      }
      // println(s"    copy remaining elements $copySourcePos -> $copyTargetPos after deleted positions")
      val sliceEnd = prev.sliceStart(idx) + prev.sliceLength(idx)
      while(copySourcePos < sliceEnd) {
        nextData(copyTargetPos) = prev.data(copySourcePos)
        copyTargetPos += 1
        copySourcePos += 1
      }
      // debug(nextData.arr)

      // println(s"    add new elements to slice at $copyTargetPos")
      while(addElemIdx < addElemLength && addElem.a(addElemIdx) == idx) {
        nextData(copyTargetPos) = addElem.b(addElemIdx)
        copyTargetPos += 1
        sliceLength += 1
        addElemIdx += 1
        // println(s"  idx: $idx (addElemIdx: $addElemIdx, delElemIdx: $delElemIdx)")
      }

      // println("    set slice length")
      nextData(sliceDataStartPos) = sliceLength
      // debug(nextData.arr)

      // now copy old slice data and skip deleted elements


      sliceDataStartPos += (1 + sliceLength)
      // debug(nextData.arr)
    }

    // println("calculate slice starts and lengths for added Idx")
    loop(sliceCount, start = prev.length) { idx => // idx covers all elements in addIdx
      nextData(idx) = sliceDataStartPos
      var sliceLength = 0 // adding a fresh slice == insertion pos for added elements
      var copyTargetPos = sliceDataStartPos+1 // pos after the length field
      // println(s"write added elements into new slices at $copyTargetPos")
      while(addElemIdx < addElemLength && addElem.a(addElemIdx) == idx) {
        nextData(copyTargetPos) = addElem.b(addElemIdx)
        copyTargetPos += 1
        sliceLength += 1
        addElemIdx += 1
        // println(s"  idx: $idx (addElemIdx: $addElemIdx, delElemIdx: $delElemIdx)")
      }

      nextData(sliceDataStartPos) = sliceLength
      sliceDataStartPos += (1 + sliceLength)
      // debug(nextData.arr)
    }
    // debug(nextData.arr)


    // println()
    // println(s"reads: ${nextData.reads}, writes: ${nextData.writes} / ${nextData.length}")
    // debug(nextData.arr)

    next
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
  @inline def empty = new NestedArrayInt(data = Array(0))

  def apply(nested: Array[Array[Int]]): NestedArrayInt = {
    val n = nested.length
    var currentStart = n
    nested.foreachElement { slice =>
      currentStart += (1 + slice.length)
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

  def apply(builders: Array[mutable.ArrayBuilder.ofInt]): NestedArrayInt = {
    // ArrayBuilders can also be null to represent an empty builder
    val n = builders.length
    var currentStart = n
    val arrays = new Array[Array[Int]](n)
    builders.foreachIndexAndElement { (i, builder) =>
      val array = if (builder == null) null else builder.result()
      arrays(i) = array
      currentStart += (if (array == null) 1 else (array.length + 1))
    }

    val dataLength = currentStart + 1 // 1 is for number of builders arrays
    val data = new Array[Int](dataLength)
    currentStart = n
    builders.foreachIndex { i =>
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
