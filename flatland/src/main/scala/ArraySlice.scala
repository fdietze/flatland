package flatland

import scala.collection.mutable

@inline final class ArraySliceInt(val array: Array[Int], val start: Int, val length: Int) extends IndexedSeq[Int] {
  @inline override def apply(idx: Int): Int = array(start + idx)
  @inline override def isEmpty = length == 0
  @inline override def slice(from: Int, until: Int): ArraySliceInt = new ArraySliceInt(array, start + from, until - from)

  override protected[this] def newBuilder: mutable.Builder[Int, IndexedSeq[Int]] = new mutable.Builder[Int, IndexedSeq[Int]] {
    val self = new mutable.ArrayBuilder.ofInt
    override def +=(elem: Int): this.type = { self += elem; this }
    override def clear(): Unit = self.clear()
    override def result(): IndexedSeq[Int] = self.result()
    override def sizeHint(size: Int): Unit = self.sizeHint(size)
  }
  override def iterator: Iterator[Int] = array.iterator.slice(start, start + length)
}

object ArraySliceInt {
  def fromArray(array: Array[Int]): ArraySliceInt = {
    new ArraySliceInt(array, 0, array.length)
  }
}
