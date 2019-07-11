package flatland.bench

import scala.concurrent.duration._
import bench._
import bench.util._
import flatland._

// to run this benchmark in JS:
// ;set mainClass in Compile := Some("flatland.bench.NestedArrayChange"); run

object NestedArrayChange {
  def generateLatticeGraph(size: Int): NestedArrayIntValues = {
    val n = Math.sqrt(size).floor.toInt
    NestedArrayInt(Array.tabulate(size){ i =>
      Array(i - 1).filter(x => x >= (i / n) * n) ++
        Array(i + 1).filter(x => x <= ((i / n) * n + n - 1) && x < size) ++
        Array(i - n).filter(x => x >= 0) ++
        Array(i + n).filter(x => x < size)
    })
  }

  def main(args: Array[String]): Unit = {
    assert(false, "assertions enabled")
    val comparison = Comparison("NestedArrayInt", Seq(
      BenchmarkImmutableInit[(NestedArrayIntValues, Int)](
        "3x addIdx",
        size => (generateLatticeGraph(size), 3),
        {
          case (nestedArray, addIdx) =>
            nestedArray.changed(addIdx = addIdx)
        }
      ),
      BenchmarkImmutableInit[(NestedArrayIntValues, InterleavedArrayInt)](
        "addElem 3x at beginning",
        size => (generateLatticeGraph(size), InterleavedArrayInt(Array(0 -> 99999, 1 -> 99999, 2 -> 99999))),
        {
          case (nestedArray, addElem) =>
            nestedArray.changed(addElem = addElem)
        }
      ),
      BenchmarkImmutableInit[(NestedArrayIntValues, InterleavedArrayInt)](
        "delElem 3x at beginning",
        size => (generateLatticeGraph(size), InterleavedArrayInt(Array(0 -> 0, 1 -> 0, 2 -> 0))),
        {
          case (nestedArray, delElem) =>
            nestedArray.changed(delElem = delElem)
        }
      ),
      BenchmarkImmutableInit[(NestedArrayIntValues, Int, InterleavedArrayInt, InterleavedArrayInt)](
        "addIdx, addElem, delElem 3x",
        size => (
          generateLatticeGraph(size),
          3,
          InterleavedArrayInt(Array(0 -> 99999, 1 -> 99999, 2 -> 99999)),
          InterleavedArrayInt(Array(0 -> 0, 1 -> 0, 2 -> 0))
        ),
        {
          case (nestedArray, addIdx, addElem, delElem) =>
            nestedArray.changed(
              addIdx = addIdx,
              addElem = addElem,
              delElem = delElem
            )
        }
      )
    ))
    runComparison(comparison, List(100, 1000, 10000, 100000), 60 seconds)

    ()
  }
}
