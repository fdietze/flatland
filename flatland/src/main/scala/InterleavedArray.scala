package flatland

object InterleavedArrayInt {
  @inline def create(n:Int): InterleavedArrayInt = new InterleavedArrayInt(new Array[Int](n*2))
}

@inline final class InterleavedArrayInt(val interleaved:Array[Int]) extends AnyVal {
  @inline def a(i:Int): Int = interleaved(i*2)
  @inline def b(i:Int): Int = interleaved(i*2+1)
  @inline def updatea(i:Int, value:Int): Unit = interleaved(i*2) = value
  @inline def updateb(i:Int, value:Int): Unit = interleaved(i*2+1) = value

  @inline def x(i:Int): Int = a(i)
  @inline def y(i:Int): Int = b(i)
  @inline def updatex(i:Int, value:Int): Unit = updatea(i,value)
  @inline def updatey(i:Int, value:Int): Unit = updateb(i,value)

  @inline def elementCount:Int = interleaved.length / 2

  @inline def foreachTwoElements(f: (Int,Int) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while(i < n ) {
      f(a(i), b(i))
      i += 1
    }
  }
  @inline def foreachIndexAndTwoElements(f: (Int,Int,Int) => Unit): Unit = {
    val n = elementCount
    var i = 0

    while(i < n ) {
      f(i, a(i), b(i))
      i += 1
    }
  }
}

