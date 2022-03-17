package flatland

import scala.collection.mutable

object ArrayQueueInt {
  @inline def create(capacity: Int) = {
    val data = new Array[Int](capacity)
    new ArrayQueueInt(capacity, data)
  }
}

@inline final class ArrayQueueInt private (
    val capacity: Int,
    val data: Array[Int],
    var start: Int = 0,
    var end: Int = 0, // exclusive
    var length: Int = 0,
) extends mutable.IndexedSeq[Int] {
  @inline override def isEmpty = length == 0
  @inline def isFull           = length == capacity

  @inline def first                                = data(start)
  @inline def lastIdx                              = (end - 1 + capacity) % capacity
  @inline override def last                        = data(lastIdx)
  @inline override def apply(idx: Int)             = data((start + idx) % capacity)
  @inline override def update(idx: Int, elem: Int) = data((start + idx) % capacity) = elem

  @inline private def incrementStart(): Unit = start = (start + 1) % capacity
  @inline private def incrementEnd(): Unit   = end = (end + 1) % capacity

  @inline def add(elem: Int): Unit = {
    data(end) = elem
    incrementEnd()

    if (length < capacity) {
      length += 1
    } else {
      incrementStart()
    }
  }
  @inline def +=(elem: Int)        = add(elem)

  @inline def popBack(): Int = {
    val elem = first
    incrementStart()
    length -= 1
    elem
  }

  @inline def foreachElement(f: Int => Unit): Unit = {
    if (length > 0) {
      if (start <= lastIdx) {
        var i = start
        while (i <= lastIdx) {
          f(data(i))
          i += 1
        }
      } else { // end < start
        var i = start
        while (i < capacity) {
          f(data(i))
          i += 1
        }
        i = 0
        while (i < start) {
          f(data(i))
          i += 1
        }
      }
    }
  }
}
