package flatland.test.property

import flatland._

import org.scalacheck._
import org.scalacheck.util.Pretty

object Helpers {
  case class NestedWithChanges(
    nested: Vector[Vector[Int]] = Vector.empty,
    addIdx: Int = 0,
    addElem: Vector[(Int, Int)] = Vector.empty,
    delElem: Vector[(Int, Int)] = Vector.empty
  ) {

    // call assertions:
    val nestedArray = NestedArrayInt(nested.map(_.toArray).toArray)
    nestedArray.changedWithAssertions(
      addIdx,
      InterleavedArrayInt(addElem),
      InterleavedArrayInt(delElem)
    )

    override def toString = s"\nNestedWithChanges(\n  nested = Vector(\n    ${nested.mkString("\n    ")})\n  addIdx = $addIdx\n  addElem = $addElem\n  delElem = $delElem\n)\n"
  }

  implicit class RichShrink[T](val shrink: Shrink[T]) extends AnyVal {
    def append(that: Shrink[T]) = Shrink {
      (t: T) => this.shrink.shrink(t) append that.shrink(t)

    }
  }

  val removeLastElemFromAddIdx: NestedWithChanges => Stream[NestedWithChanges] = {
    case n @ NestedWithChanges(nested, addIdx, addElem, delElem) =>
      (if (addIdx > 0)
        Stream({
        val shrinkedAddIdx = addIdx - 1
        // remove invalid addElem
        val filteredAddElem = addElem.filter(_._1 < nested.length + shrinkedAddIdx)
        NestedWithChanges(nested, shrinkedAddIdx, filteredAddElem, delElem)
      }, {
        val shrinkedAddIdx = addIdx - 1
        // re-map invalid addElem
        val shiftedAddElem = addElem.collect{
          case (idx, elem) if idx > 0 && idx == nested.length + addIdx - 1 => (idx - 1) -> elem
          case (idx, elem) if idx != 0                      => idx -> elem
        }
        NestedWithChanges(nested, shrinkedAddIdx, shiftedAddElem, delElem)
      }, {
        val shrinkedAddIdx = addIdx - 1
        // shift all addElem down
        val shiftedAddElem = addElem.collect{
          case (idx, elem) if idx > 0 => (idx - 1) -> elem
        }
        NestedWithChanges(nested, shrinkedAddIdx, shiftedAddElem, delElem)
      })

      else Stream.empty)
  }

  val removeOneElementFromAddElem: NestedWithChanges => Stream[NestedWithChanges] = {
    case NestedWithChanges(nested, addIdx, addElem, delElem) =>
      for {
        i <- (0 until addElem.length).toStream
      } yield {
        val shrinkedAddElem = addElem.take(i) ++ addElem.drop(i + 1)
        NestedWithChanges(nested, addIdx, shrinkedAddElem, delElem)
      }
  }

  val removeOneElementFromDelElem: NestedWithChanges => Stream[NestedWithChanges] = {
    case NestedWithChanges(nested, addIdx, addElem, delElem) =>
      for {
        i <- (0 until delElem.length).toStream
      } yield {
        val shrinkedDelElem = delElem.take(i) ++ delElem.drop(i + 1)
        NestedWithChanges(nested, addIdx, addElem, shrinkedDelElem)
      }
  }

  val shrinkValueInAddElem: NestedWithChanges => Stream[NestedWithChanges] = {
    case NestedWithChanges(nested, addIdx, addElem, delElem) =>
      for {
        i <- (0 until addElem.length).toStream
        shrinkedValue <- implicitly[Shrink[Int]].shrink(addElem(i)._2)
      } yield {
        val shrinkedAddElem = addElem.updated(i, addElem(i)._1 -> shrinkedValue)
        NestedWithChanges(nested, addIdx, shrinkedAddElem, delElem)
      }
  }

  val shrinkValueInNested: NestedWithChanges => Stream[NestedWithChanges] = {
    case NestedWithChanges(nested, addIdx, addElem, delElem) =>
      for {
        idx <- (0 until nested.length).toStream
        i <- (0 until nested(idx).length).toStream
        shrinkedValue <- implicitly[Shrink[Int]].shrink(nested(idx)(i))
      } yield {
        val shrinkedNested = nested.updated(idx, nested(idx).updated(i, shrinkedValue))
        NestedWithChanges(shrinkedNested, addIdx, addElem, delElem)
      }
  }

  val removeIdxFromNested: NestedWithChanges => Stream[NestedWithChanges] = {
    case NestedWithChanges(nested, addIdx, addElem, delElem) =>
      for {
        removedIdx <- (0 until nested.length).toStream
      } yield {
        val shrinkedNested = nested.take(removedIdx) ++ nested.drop(removedIdx + 1)

        val shiftedAddElem = addElem.collect{
          case (addIdx, elem) if addIdx < removedIdx                 => addIdx -> elem
          case (addIdx, elem) if (addIdx > 0 && addIdx > removedIdx) => (addIdx - 1) -> elem
        }
        val shiftedDelElem = delElem.collect{
          case (delIdx, elem) if delIdx < removedIdx => delIdx -> elem
          case (delIdx, elem) if delIdx > removedIdx => (delIdx - 1) -> elem
        }
        NestedWithChanges(shrinkedNested, addIdx, shiftedAddElem, shiftedDelElem)
      }
  }

  val removeInnerElemFromNested: NestedWithChanges => Stream[NestedWithChanges] = {
    case NestedWithChanges(nested, addIdx, addElem, delElem) =>
      for {
        idx <- (0 until nested.length).toStream
        i <- (0 until nested(idx).length).toStream
      } yield {
        val shrinkedNested = nested.updated(idx, nested(idx).take(i) ++ nested(idx).drop(i + 1))
        val shrinkedDelElem = delElem.collect{
          case (delIdx, pos) if delIdx != idx            => delIdx -> pos
          case (delIdx, pos) if delIdx == idx && pos < i => delIdx -> pos
          case (delIdx, pos) if delIdx == idx && pos > i => delIdx -> (pos - 1)
        }
        NestedWithChanges(shrinkedNested, addIdx, addElem, shrinkedDelElem)
      }
  }

}

import org.scalatest.FreeSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.propBoolean
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import Helpers._

class NestedArrayChangePropertySpec extends FreeSpec with Checkers {

  val nestedArrays = arbitrary[Vector[Vector[Int]]]
  val nestedArraysWithChanges = for {
    nested <- nestedArrays

    addIdx <- Gen.sized { size => size }

    addElem <- if (nested.length + addIdx == 0)
      Gen.const(Vector.empty)
    else
      Gen.containerOf[Vector, (Int, Int)](for {
        idx <- Gen.choose(0, nested.length + addIdx - 1)
        elem <- arbitrary[Int]
      } yield (idx, elem)).map(_.sorted)

    delElem <- for {
      sliceSubsetWithIndex <- Gen.someOf(nested.zipWithIndex)
      delElem <- Gen.sequence[Vector[Vector[(Int, Int)]], Vector[(Int, Int)]](
        sliceSubsetWithIndex.map {
          case (slice, idx) =>
            Gen.someOf(slice.indices.map(idx -> _)).map(_.toVector)
        }
      ).map(_.flatten.sorted)
    } yield delElem
  } yield NestedWithChanges(nested, addIdx, addElem, delElem)

  ////
  assert(removeLastElemFromAddIdx(
    NestedWithChanges(Vector(), 3, Vector(1 -> 5, 2 -> 3))
  ).toList == List(
      NestedWithChanges(Vector(), 2, Vector(1 -> 5)),
      NestedWithChanges(Vector(), 2, Vector(1 -> 5, 1 -> 3)),
      NestedWithChanges(Vector(), 2, Vector(0 -> 5, 1 -> 3))
    ))

  ////
  assert(removeLastElemFromAddIdx(
    NestedWithChanges(Vector(), 1, Vector(0 -> 5))
  ).toList == List(
      NestedWithChanges(Vector(), 0, Vector()),
      NestedWithChanges(Vector(), 0, Vector()),
      NestedWithChanges(Vector(), 0, Vector())
    ))

  ////
  assert(removeOneElementFromAddElem(
    NestedWithChanges(Vector(), 3, Vector(1 -> 5, 2 -> 3))
  ).head ==
    NestedWithChanges(Vector(), 3, Vector(2 -> 3)))

  assert(removeOneElementFromAddElem(
    NestedWithChanges(Vector(), 3, Vector(2 -> 3))
  ).head ==
    NestedWithChanges(Vector(), 3, Vector()))

  ////
  assert(shrinkValueInAddElem(
    NestedWithChanges(Vector(), 3, Vector(1 -> 5, 2 -> 3))
  ).head ==
    NestedWithChanges(Vector(), 3, Vector(1 -> implicitly[Shrink[Int]].shrink(5).head, 2 -> 3)))

  ////
  assert(shrinkValueInNested(
    NestedWithChanges(Vector(Vector(17)), 2, Vector(1 -> 5, 2 -> 3))
  ).head ==
    NestedWithChanges(Vector(Vector(implicitly[Shrink[Int]].shrink(17).head)), 2, Vector(1 -> 5, 2 -> 3)))

  assert(removeIdxFromNested(
    NestedWithChanges(Vector(Vector(2), Vector(3, 5)), 1, Vector(1 -> 4), Vector(0 -> 0, 1 -> 1))
  ).head ==
    NestedWithChanges(Vector(Vector(3, 5)), 1, Vector(0 -> 4), Vector(0 -> 1)))

  assert(removeIdxFromNested(
    NestedWithChanges(Vector(Vector(2)), 0, Vector(0 -> 4))
  ).head ==
    NestedWithChanges(Vector(), 0, Vector()))

  ////
  assert(removeInnerElemFromNested(
    NestedWithChanges(Vector(Vector(1, 7, 9), Vector(2, 5, 3, 4)), 1, Vector(0 -> 5, 2 -> 4), Vector(0 -> 0, 0 -> 2, 1 -> 3))
  ).take(2).toList == List(
      NestedWithChanges(Vector(Vector(7, 9), Vector(2, 5, 3, 4)), 1, Vector(0 -> 5, 2 -> 4), Vector(0 -> 1, 1 -> 3)),
      NestedWithChanges(Vector(Vector(1, 9), Vector(2, 5, 3, 4)), 1, Vector(0 -> 5, 2 -> 4), Vector(0 -> 0, 0 -> 1, 1 -> 3))
    ))

  implicit def shrinkNestedWithChanges(implicit shrinkInt: Shrink[Int]): Shrink[NestedWithChanges] = Shrink { n =>
    Stream.empty
      .append(removeLastElemFromAddIdx(n))
      .append(removeOneElementFromAddElem(n))
      .append(shrinkValueInAddElem(n))
      .append(removeOneElementFromDelElem(n))
      .append(removeIdxFromNested(n))
      .append(removeInnerElemFromNested(n))
      .append(shrinkValueInNested(n))
  }

  check (
    forAll(nestedArraysWithChanges) {
      case NestedWithChanges(nestedVector, addIdx, addElemVector, delElemVector) =>
        val nested = nestedVector.map(_.toArray).toArray
        val addElem = addElemVector.toArray
        val delElem = delElemVector.toArray

        val changedArray = {
          val result = nested ++ Array.fill(addIdx)(Array.empty[Int])
          addElem.foreach{
            case (idx, elem) => result(idx) :+= elem
          }
          delElem.reverseIterator.foreach{
            case (idx, pos) => result(idx) = (result(idx).take(pos) ++ result(idx).drop(pos + 1))
          }
          result
        }
        val original = NestedArrayInt(nested)
        val result = original.changedWithAssertions(
          addIdx,
          InterleavedArrayInt(addElem),
          InterleavedArrayInt(delElem)
        )
        val expected = NestedArrayInt(changedArray.toArray)

        result.data.sameElements(expected.data)
    },
    MinSuccessful(10000), SizeRange(50)
  )
}
