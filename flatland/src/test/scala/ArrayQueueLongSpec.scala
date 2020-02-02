package flatland.test

import flatland._

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ArrayQueueLongSpec extends AnyFreeSpec with Matchers {

  "ArrayQueueLong" - {
    def foreachElementCollect(queue: ArrayQueueLong): List[Long] = {
      val builder = List.newBuilder[Long]
      queue.foreachElement(builder += _)
      builder.result()
    }

    "everything" in {
      val queue = ArrayQueueLong.create(3)
      queue.size mustEqual 0
      queue.isEmpty mustEqual true
      queue.isFull mustEqual false
      queue.toList mustEqual List()

      queue += 4
      queue.size mustEqual 1
      queue.isEmpty mustEqual false
      queue.isFull mustEqual false
      queue.first == 4
      queue.last == 4
      queue(0) == 4
      foreachElementCollect(queue) mustEqual List(4)
      queue.toList mustEqual List(4)

      queue += 7
      queue.size mustEqual 2
      queue.isEmpty mustEqual false
      queue.isFull mustEqual false
      queue.first == 4
      queue.last == 7
      queue(0) == 4
      queue(1) == 7
      foreachElementCollect(queue) mustEqual List(4, 7)
      queue.toList mustEqual List(4, 7)

      // first pop
      queue.popBack() mustEqual 4
      queue.size mustEqual 1
      queue.isEmpty mustEqual false
      queue.isFull mustEqual false
      queue.first == 7
      queue.last == 7
      queue(0) == 7
      foreachElementCollect(queue) mustEqual List(7)
      queue.toList mustEqual List(7)

      // first time full: [first, _, last]
      queue += 9
      queue += 3
      queue.size mustEqual 3
      queue.isEmpty mustEqual false
      queue.isFull mustEqual true
      queue.first == 7
      queue.last == 3
      queue(0) == 7
      queue(1) == 9
      queue(2) == 3
      foreachElementCollect(queue) mustEqual List(7, 9, 3)
      queue.toList mustEqual List(7, 9, 3)

      // wrap around by one: [last, first, _]
      queue += 4
      queue.size mustEqual 3
      queue.isEmpty mustEqual false
      queue.isFull mustEqual true
      queue.first == 9
      queue.last == 4
      queue(0) == 9
      queue(1) == 3
      queue(2) == 4
      foreachElementCollect(queue) mustEqual List(9, 3, 4)
      queue.toList mustEqual List(9, 3, 4)

      // wrap around by one: [_, last, first]
      queue += 5
      queue.size mustEqual 3
      queue.isEmpty mustEqual false
      queue.isFull mustEqual true
      queue.first == 3
      queue.last == 5
      queue(0) == 3
      queue(1) == 4
      queue(2) == 5
      foreachElementCollect(queue) mustEqual List(3, 4, 5)
      queue.toList mustEqual List(3, 4, 5)

      // wrap around start by popBack: [first, last, _]
      queue.popBack() mustEqual 3
      queue.size mustEqual 2
      queue.isEmpty mustEqual false
      queue.isFull mustEqual false
      queue.first == 4
      queue.last == 5
      queue(0) == 4
      queue(1) == 5
      foreachElementCollect(queue) mustEqual List(4, 5)
      queue.toList mustEqual List(4, 5)

      // popBack
      queue.popBack() mustEqual 4
      queue.size mustEqual 1
      queue.isEmpty mustEqual false
      queue.isFull mustEqual false
      queue.first == 5
      queue.last == 5
      queue(0) == 5
      foreachElementCollect(queue) mustEqual List(5)
      queue.toList mustEqual List(5)

      // popBack to empty in center of data [_, first/lastIdx, _]
      queue.popBack() mustEqual 5
      queue.size mustEqual 0
      queue.isEmpty mustEqual true
      queue.isFull mustEqual false
      foreachElementCollect(queue) mustEqual List()
      queue.toList mustEqual List()

      // add first element in the center
      queue += 4
      queue.size mustEqual 1
      queue.isEmpty mustEqual false
      queue.isFull mustEqual false
      queue.first == 4
      queue.last == 4
      queue(0) == 4
      foreachElementCollect(queue) mustEqual List(4)
      queue.toList mustEqual List(4)
    }
  }
}
