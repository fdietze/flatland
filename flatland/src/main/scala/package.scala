import scala.collection.mutable
import scala.reflect.ClassTag

package object flatland {
  @inline def loop(n: Int, start: Int = 0)(f: Int => Unit): Unit = {
    var i = start
    while (i < n) {
      f(i)
      i += 1
    }
  }

  implicit final class RichIndexedSeq[T](val self: IndexedSeq[T]) extends AnyVal {
    @inline def minMax(smallerThan: (T, T) => Boolean): (T, T) = {
      if (self.isEmpty) throw new UnsupportedOperationException("minMax on empty sequence")

      var min: T = self(0)
      var max: T = min

      var i = 1
      while (i < self.length) {
        val value = self(i)
        if (smallerThan(value, min)) min = value
        if (smallerThan(max, value)) max = value
        i += 1
      }

      (min, max)
    }

    //    @inline def filterIdx(p: Int => Boolean)(implicit ev: ClassTag[T]):Array[T] = {
    //      val builder = new mutable.ArrayBuilder.ofRef[T]
    //      var i = 0
    //      while(i < array.length) {
    //        if(p(i))
    //          builder += array(i)
    //        i += 1
    //      }
    //      builder.result()
    //    }

    @inline def foreachIndex(f: Int => Unit): Unit = {
      val n = self.length
      var i = 0

      while (i < n) {
        f(i)
        i += 1
      }
    }

    @inline def foreachElement(f: T => Unit): Unit = {
      val n = self.length
      var i = 0

      while (i < n) {
        f(self(i))
        i += 1
      }
    }

    @inline def foreachIndexAndElement(f: (Int, T) => Unit): Unit = {
      val n = self.length
      var i = 0

      while (i < n) {
        f(i, self(i))
        i += 1
      }
    }
  }

  implicit final class RichArray[T](val array: Array[T]) extends AnyVal {
    @inline def get(idx: Int): Option[T] = if (0 <= idx && idx < array.length) Some(array(idx)) else None

    @inline def filterIdx(p: Int => Boolean)(implicit ev: ClassTag[T]): Array[T] = {
      val builder = mutable.ArrayBuilder.make[T]
      array.foreachIndexAndElement{ (i, elem) =>
        if (p(i)) builder += elem
      }
      builder.result()
    }

    @inline def findIdx(p: T => Boolean): Option[Int] = {
      array.foreachIndexAndElement{ (i, elem) =>
        if (p(elem)) return Some(i)
      }
      None
    }

    @inline def findIdxByIdx(p: Int => Boolean): Option[Int] = {
      array.foreachIndex{ i =>
        if (p(i)) return Some(i)
      }
      None
    }

    @inline def filterIdxToArraySet(p: Int => Boolean): (ArraySet, Int) = {
      val set = ArraySet.create(array.length)
      var i = 0
      var size = 0
      while (i < array.length) {
        if (p(i)) {
          set += i
          size += 1
        }
        i += 1
      }
      (set, size)
    }

    @inline def foreachIndex(f: Int => Unit): Unit = {
      loop(array.length)(f)
    }

    @inline def foreachElement(f: T => Unit): Unit = {
      val n = array.length
      var i = 0

      while (i < n) {
        f(array(i))
        i += 1
      }
    }

    @inline def foreachIndexAndElement(f: (Int, T) => Unit): Unit = {
      val n = array.length
      var i = 0

      while (i < n) {
        f(i, array(i))
        i += 1
      }
    }

    @inline def foreachIndex2Combination(f: (Int, Int) => Unit): Unit = {
      val n = array.length
      var i = 0
      var j = 0

      while (i < n) {
        j = i
        while (j < n) {
          if (i != j) f(i, j)
          j += 1
        }
        i += 1
      }
    }

    @inline def mapWithIndex[R: ClassTag](f: (Int, T) => R): Array[R] = {
      val n = array.length
      var i = 0
      val result = new Array[R](n)

      while (i < n) {
        result(i) = f(i, array(i))
        i += 1
      }

      result
    }
  }

  implicit final class RichIntArray(val array: Array[Int]) extends AnyVal {
    @inline def filterIndex(p: Int => Boolean): Array[Int] = {
      val builder = new mutable.ArrayBuilder.ofInt
      var i = 0
      while (i < array.length) {
        if (p(i))
          builder += array(i)
        i += 1
      }
      builder.result()
    }

    @inline def toArraySet(n: Int): ArraySet = {
      val marked = ArraySet.create(n)
      marked.add(array)
      marked
    }
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
    vertexCount: Int,
    foreachSuccessor: (Int, Int => Unit) => Unit, // (idx, f) => successors(idx).foreach(f)
    init: (Int => Unit, ArrayStackInt) => Unit, // (enqueue,_) => enqueue(start)
    processVertex: Int => PROCESSRESULT, // result += _
    loopConditionGuard: (() => Boolean) => Boolean = loopConditionGuardDefault,
    advanceGuard: (PROCESSRESULT, () => Unit) => Unit = advanceGuardDefault,
    enqueueGuard1: (Int, () => Unit) => Unit = enqueueGuardDefault,
    enqueueGuard2: (Int, () => Unit) => Unit = enqueueGuardDefault
  ): Unit = {
    val stack = ArrayStackInt.create(capacity = vertexCount)
    val visited = ArraySet.create(vertexCount)

    @inline def enqueue1(elem: Int): Unit = {
      enqueueGuard1(elem, { () =>
        stack.push(elem)
        visited += elem
      })
    }

    @inline def enqueue2(elem: Int): Unit = {
      enqueueGuard2(elem, { () =>
        stack.push(elem)
        visited += elem
      })
    }

    init(enqueue1, stack)
    while (loopConditionGuard(() => !stack.isEmpty)) {
      val current = stack.pop()
      visited += current

      advanceGuard(
        processVertex(current),
        () =>
          foreachSuccessor(current, { next =>
            if (visited.containsNot(next)) {
              enqueue2(next)
            }
          })
      )
    }
  }
}
