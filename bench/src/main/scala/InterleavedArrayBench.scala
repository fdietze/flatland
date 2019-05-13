package flatland.bench

import scala.concurrent.duration._
import bench._
import bench.util._
import flatland._
// set mainClass in Compile := Some("flatland.bench.InterleavedArray")
object InterleavedArray {

  // def main(args: Array[String]): Unit = {
  //   assert(false, "assertions enabled")
  //   val comparison = Comparison("InterleavedArrayInt", Seq(
  //     BenchmarkImmutableInit[InterleavedArrayInt](
  //       "exists",
  //       size => InterleavedArrayInt(Array.tabulate(size)(i => (size - i - 1, size - i - 1))),
  //       { interleaved =>
  //         interleaved.exists((a, _) => a == 0)
  //       }
  //     ),
  //   // def exists2(f:(Int,Int) => Boolean):Boolean = {
  //   //   flatland.exists2(elementCount)(i => f(a(i), b(i)))
  //   // }
  //   // BenchmarkImmutableInit[InterleavedArrayInt](
  //   //   "exists2",
  //   //   size => InterleavedArrayInt(Array.tabulate(size)(i => (size - i - 1, size - i - 1))),
  //   //   { interleaved =>
  //   //     interleaved.exists2((a, _) => a == 0)
  //   //   }
  //   // ),
  //   ))
  //   runComparison(comparison, List(100, 1000, 10000), 120 seconds)

  //   ()
  // }
}
