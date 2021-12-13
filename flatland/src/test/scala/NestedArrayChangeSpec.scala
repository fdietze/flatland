package flatland.test

import flatland._
import collection.mutable

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class NestedArrayChangeSpec extends AnyFreeSpec with Matchers {

  "NestedArrayInt" - {
    def debug(arr: Array[Int]) = {
      println(arr.map(x => f"$x%03d").mkString("  "))
    }

    "add idx to empty" in {
      val nested = NestedArrayInt.empty

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add two idx to empty" in {
      val nested = NestedArrayInt.empty

      val patched  = nested.changedWithAssertions(
        addIdx = 2,
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](),
          /* 1 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add idx with elem to empty" in {
      val nested = NestedArrayInt.empty

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
        addElem = InterleavedArrayInt((0, 22)), // already exists
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add elem to existing slice" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addElem = InterleavedArrayInt((1, 999)),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44, 999),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new idx" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }
    "add three new idx" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 3,
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](),
          /* 4 -> */ Array[Int](),
          /* 5 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new idx and elem" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
        addElem = InterleavedArrayInt((1, 999)),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44, 999),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new idx and elem at new idx" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
        addElem = InterleavedArrayInt((3, 999)),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](999),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new idx with two elems at new idx" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
        addElem = InterleavedArrayInt((3, 999), (3, 888)),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](999, 888),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new idx with two elems, one at new idx" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 1,
        addElem = InterleavedArrayInt((1, 888), (3, 999)),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44, 888),
          /* 2 -> */ Array[Int](66, 22),
          /* 3 -> */ Array[Int](999),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new elems at different idx" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addElem = InterleavedArrayInt((1, 999), (2, 888)),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44, 999),
          /* 2 -> */ Array[Int](66, 22, 888),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add new elems: don't overwrite following slice lengths (found by scalacheck)" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](),
          /* 1 -> */ Array[Int](),
          /* 2 -> */ Array[Int](0),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addElem = InterleavedArrayInt(0 -> 0),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](0),
          /* 1 -> */ Array[Int](),
          /* 2 -> */ Array[Int](0),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete elem and make slice empty" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(0 -> 0),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete elem and make slice empty 2 (found by scalacheck)" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](0),
          /* 1 -> */ Array[Int](),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(0 -> 0),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](),
          /* 1 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete two elements from different slices (found by scalacheck)" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](22, 33),
          /* 1 -> */ Array[Int](44, 55),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(0 -> 0, 1 -> 1),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](33),
          /* 1 -> */ Array[Int](44),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete elem at end of slice" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(1 -> 2),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete elem at beginning of slice" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(1 -> 0),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete elem at middle of slice" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(1 -> 1),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete two elems from same slice" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(1 -> 1, 1 -> 2),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete two elems from different slices" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(1 -> 1, 2 -> 1),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 44),
          /* 2 -> */ Array[Int](66),
        ),
      )

      patched.data mustEqual expected.data
    }

    "delete three elems from same array (inner length shrinks below last sliceLength)" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(1 -> 0, 1 -> 1, 1 -> 2),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add and delete two elems at different slices" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](77),
          /* 1 -> */ Array[Int](55, 33, 44),
          /* 2 -> */ Array[Int](66, 22),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addIdx = 2,
        addElem = InterleavedArrayInt(0 -> 88, 2 -> 11, 4 -> 99),
        delElem = InterleavedArrayInt(0 -> 0, 1 -> 1),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](88),
          /* 1 -> */ Array[Int](55, 44),
          /* 2 -> */ Array[Int](66, 22, 11),
          /* 3 -> */ Array[Int](),
          /* 4 -> */ Array[Int](99),
        ),
      )

      patched.data mustEqual expected.data
    }

    "add and del one elem from same slice (found by scalacheck)" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](0),
        ),
      )

      val patched  = nested.changedWithAssertions(
        addElem = InterleavedArrayInt(0 -> 0),
        delElem = InterleavedArrayInt(0 -> 0),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](0),
        ),
      )

      patched.data mustEqual expected.data
    }

    "del both elems from first slice (found by scalacheck)" in {
      val nested = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](22, 44),
        ),
      )

      val patched  = nested.changedWithAssertions(
        delElem = InterleavedArrayInt(0 -> 0, 0 -> 1),
      )
      val expected = NestedArrayInt(
        Array(
          /* 0 -> */ Array[Int](),
        ),
      )

      patched.data mustEqual expected.data
    }

  }
}
