package flatland

object ArrayStackInt {
  @inline def create(capacity: Int) = new ArrayStackInt(new Array[Int](capacity))
}

@inline final class ArrayStackInt private (val stack: Array[Int], var top: Int = 0) {
  @inline private def incrementTop(): Unit = top += 1
  @inline private def decrementTop(): Unit = top -= 1

  @inline def size = top
  @inline def isEmpty = top == 0
  @inline def isFull = top == stack.length

  @inline def push(value: Int) = {
    stack(top) = value
    incrementTop()
  }
  @inline def pop() = {
    decrementTop()
    stack(top)
  }
}
