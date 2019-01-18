package flatland

import scala.reflect.ClassTag

object InterleavedArray {
  @inline def create[T:ClassTag](n:Int): InterleavedArray[T] = new InterleavedArray[T](new Array[T](n*2))
}

@inline final class InterleavedArray[@specialized(Int, Double, Boolean) T](val interleaved:Array[T]) {
  @inline def a(i:Int): T = interleaved(i*2)
  @inline def b(i:Int): T = interleaved(i*2+1)
  @inline def updatea(i:Int, value:T): Unit = interleaved(i*2) = value
  @inline def updateb(i:Int, value:T): Unit = interleaved(i*2+1) = value

  @inline def x(i:Int): T = a(i)
  @inline def y(i:Int): T = b(i)
  @inline def updatex(i:Int, value:T): Unit = updatea(i,value)
  @inline def updatey(i:Int, value:T): Unit = updateb(i,value)

  @inline def left(i:Int): T = a(i)
  @inline def right(i:Int): T = b(i)
  @inline def updateLeft(i:Int, value:T): Unit = updatea(i,value)
  @inline def updateRight(i:Int, value:T): Unit = updateb(i,value)

  @inline def elementCount:Int = interleaved.length / 2

  @inline def foreachTwoElements(f: (T,T) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while(i < n ) {
      f(a(i), b(i))
      i += 1
    }
  }
  @inline def foreachIndexAndTwoElements(f: (Int,T,T) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while(i < n ) {
      f(i, a(i), b(i))
      i += 1
    }
  }
}

