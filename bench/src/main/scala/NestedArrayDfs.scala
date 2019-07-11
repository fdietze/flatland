package flatland.bench

import scala.concurrent.duration._
import bench._
import bench.util._
import flatland._
import collection.mutable

// to run this benchmark in JS:
// ;set mainClass in Compile := Some("flatland.bench.NestedArrayDfs"); run
object NestedArrayDfs {
  def generateLatticeLookup(size: Int): NestedArrayInt = {
    val n = Math.sqrt(size).floor.toInt
    NestedArrayInt(Array.tabulate(size){ i =>
      Array(i - 1).filter(x => x >= (i / n) * n) ++
        Array(i + 1).filter(x => x <= ((i / n) * n + n - 1) && x < size) ++
        Array(i - n).filter(x => x >= 0) ++
        Array(i + n).filter(x => x < size)
    })
  }
   
  def generateLatticeGraph(size: Int): (NestedArrayInt, InterleavedArrayInt, NestedArrayInt) = {
    // indirect in the sense that the nestedArray stores edge indices (instead of vertex indices)
    // it is then mapped to lookup the node index in the edge array
    val successors = generateLatticeLookup(size)
    val edgesBuilder = new mutable.ArrayBuilder.ofRef[(Int, Int)]
    val incidenceBuilder = Array.fill(successors.size)(new mutable.ArrayBuilder.ofInt)
    var edgeIdx = 0
    successors.foreachIndexAndSlice { (sourceIdx, targets) =>
      targets.foreachElement { targetIdx =>
        edgesBuilder += (sourceIdx -> targetIdx)
        incidenceBuilder(sourceIdx) += edgeIdx

        edgeIdx += 1
      }
    }
    val edges = InterleavedArrayInt(edgesBuilder.result)
    val incidenceLookup = NestedArrayInt(incidenceBuilder)
    // val indirectIncidenceLookup = NestedArrayInt(incidenceBuilder).viewMapInt(edgeIdx => edges.right(edgeIdx))

    // if (successors.map(_.toList) != incidenceLookup.map(_.toList)) throw new Exception("Not Equal!")

    (successors, edges, incidenceLookup)
  }

  def main(args: Array[String]): Unit = {
    assert(false, "assertions enabled")
    val comparison = Comparison("NestedArrayInt", Seq(
      BenchmarkImmutableInit[(NestedArrayInt, InterleavedArrayInt, NestedArrayInt)](
        "NestedArrayIntValues",
        size => generateLatticeGraph(size),
        {
          case (lookup, _, _) =>
            // println("dfs1 start")
            lookup.depthFirstSearchToArray(0)
          // println("dfs1 end")
        }
      ),
      // BenchmarkImmutableInit[(NestedArrayInt, InterleavedArrayInt, NestedArrayInt)](
      //   "NestedArrayInt generic dfs",
      //   size => generateLatticeGraph(size),
      //   {
      //     case (lookup, _, _) =>
      //       val builder = new mutable.ArrayBuilder.ofInt

      //       flatland.depthFirstSearchGeneric(
      //         vertexCount = lookup.length,
      //         foreachSuccessor = (idx, f) => lookup.foreachElement(idx)(f),
      //         init = (stack, _) => stack.push(0),
      //         processVertex = builder += _,
      //         loopConditionGuard = condition => condition(),
      //         advanceGuard = (result: scala.collection.mutable.ArrayBuilder.ofInt, advance: () => Unit) => advance(),
      //         enqueueGuard = (elem, enqueue) => enqueue()
      //       )

      //       builder.result()
      //   }
      // ),
      BenchmarkImmutableInit[(NestedArrayInt, InterleavedArrayInt, NestedArrayInt)](
        "NestedArrayInt indirect generic dfs",
        size => generateLatticeGraph(size),
        {
          case (_, edges, lookup) =>
            val builder = new mutable.ArrayBuilder.ofInt

            flatland.depthFirstSearchGeneric(
              vertexCount = lookup.length,
              foreachSuccessor = (idx, f) => lookup.foreachElement(idx)(edgeIdx => f(edges.right(edgeIdx))),
              init = (stack, _) => stack.push(0),
              processVertex = builder += _,
              loopConditionGuard = condition => condition(),
              advanceGuard = (result: scala.collection.mutable.ArrayBuilder.ofInt, advance: () => Unit) => advance(),
              enqueueGuard = (elem, enqueue) => enqueue()
            )

            builder.result()
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
    runComparison(comparison, List(100, 1000, 10000, 1000000), 60 seconds)

    ()
  }
}
