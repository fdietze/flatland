package flatland

import scala.collection.mutable

object ArrayQueueLong {
  @inline def create(capacity: Int) = {
    val data = new Array[Long](capacity)
    new ArrayQueueLong(capacity, data)
  }
}

@inline final class ArrayQueueLong private (
    val capacity: Int,
    val data: Array[Long],
    var start: Int = 0,
    var end: Int = 0, // exclusive
    var length: Int = 0,
) extends mutable.IndexedSeq[Long] {
  @inline override def isEmpty = length == 0
  @inline def isFull           = length == capacity

  @inline def first                                 = data(start)
  @inline def lastIdx                               = (end - 1 + capacity) % capacity
  @inline override def last                         = data(lastIdx)
  @inline override def apply(idx: Int)              = data((start + idx) % capacity)
  @inline override def update(idx: Int, elem: Long) = data((start + idx) % capacity) = elem

  @inline private def incrementStart(): Unit = start = (start + 1) % capacity
  @inline private def incrementEnd(): Unit   = end = (end + 1) % capacity

  @inline def add(elem: Long): Unit = {
    data(end) = elem
    incrementEnd()

    if (length < capacity) {
      length += 1
    } else {
      incrementStart()
    }
  }
  @inline def +=(elem: Long)        = add(elem)

  @inline def popBack(): Long = {
    val elem = first
    incrementStart()
    length -= 1
    elem
  }

  @inline def foreachElement(f: Long => Unit): Unit = {
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
