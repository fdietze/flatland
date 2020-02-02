package flatland.test

import flatland._

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ArraySetSpec extends AnyFreeSpec with Matchers {

  "ArraySet" - {
    "add" in {
      val set = ArraySet.create(2)
      set += 0
      assert(set.contains(0))
      assert(set.containsNot(1))
    }
    "remove" in {
      val set = ArraySet.create(2)
      set += 0
      set -= 0
      assert(set.containsNot(0))
      assert(set.containsNot(1))
    }
    "clear" in {
      val set = ArraySet.create(2)
      set += 0
      set.clear()
      assert(set.containsNot(0))
      assert(set.containsNot(1))
    }
  }
}
