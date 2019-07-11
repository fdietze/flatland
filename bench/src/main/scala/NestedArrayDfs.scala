package flatland.bench

import scala.concurrent.duration._
import bench._
import bench.util._
import flatland._
import collection.mutable

// to run this benchmark in JS:
// ;set mainClass in Compile := Some("flatland.bench.NestedArrayDfs"); run
object NestedArrayDfs {
  def generateLatticeGraph(size: Int): NestedArrayInt = {
    val n = Math.sqrt(size).floor.toInt
    NestedArrayInt(Array.tabulate(size){ i =>
      Array(i - 1).filter(x => x >= (i / n) * n) ++
        Array(i + 1).filter(x => x <= ((i / n) * n + n - 1) && x < size) ++
        Array(i - n).filter(x => x >= 0) ++
        Array(i + n).filter(x => x < size)
    })
  }
  // def generateIndirectLatticeGraph(size: Int): NestedArrayInt = {
  //   // indirect in the sense that the nestedArray stores edge indices (instead of vertex indices)
  //   // it is then mapped to lookup the node index in the edge array
  //   val successors = generateLatticeGraph(size)
  //   val edgesBuilder = new mutable.ArrayBuilder.ofRef[(Int,Int)]
  //   val incidenceBuilder = Array.fill(successors.size)(new mutable.ArrayBuilder.ofInt)
  //   var edgeIdx = 0
  //   successors.foreachIndexAndSlice { (sourceIdx, targets) =>
  //     targets.foreachElement { targetIdx =>
  //       edgesBuilder += (sourceIdx -> targetIdx)
  //       incidenceBuilder(sourceIdx) += edgeIdx

  //       edgeIdx += 1
  //     }
  //   }
  //   val edges = InterleavedArrayInt(edgesBuilder.result)
  //   val incidenceLookup = NestedArrayInt(incidenceBuilder).viewMapInt(edgeIdx => edges.right(edgeIdx))


  //   if(successors.map(_.toList) != incidenceLookup.map(_.toList)) throw new Exception("Not Equal!")

  //   incidenceLookup
  // }

  def main(args: Array[String]): Unit = {
    assert(false, "assertions enabled")
    val comparison = Comparison("NestedArrayInt", Seq(
      BenchmarkImmutableInit[NestedArrayInt](
        "NestedArrayIntValues",
        size => generateLatticeGraph(size),
        {
          nestedArray =>
            // println("dfs1 start")
            nestedArray.depthFirstSearchToArray(0)
            // println("dfs1 end")
        }
      ),
      // BenchmarkImmutableInit[NestedArrayInt](
      //   "NestedArrayIntMapped",
      //   size => generateIndirectLatticeGraph(size),
      //   {
      //     nestedArray =>
      //       // println("dfs2 start")
      //       nestedArray.depthFirstSearchToArray(0)
      //       // println("dfs2 end")
      //   }
      // ),
    ))
    runComparison(comparison, List(100, 1000, 10000), 60 seconds)

    ()
  }
}
