# flatland

High-performance helpers for working with flat arrays in Scala.

Be aware that `@inline class` is only optimized in ScalaJS.
To know more about the JVM, you should read: https://developer.lightbend.com/blog/2018-11-01-the-scala-2.12-2.13-inliner-and-optimizer/index.html

## Dependency

With [JitPack](https://jitpack.io), it is common to point to a specific commit to make your builds reproducible:

```scala
resolvers += ("jitpack" at "https://jitpack.io")
libraryDependencies += "com.github.fdietze.flatland" %% "flatland" % "7d78f60"

// With ScalaJS:
libraryDependencies += "com.github.fdietze.flatland" %%% "flatland" % "7d78f60"
```

To use it in Ammonite:

```scala
import $repo.`https://jitpack.io`
import $ivy.`com.github.fdietze.flatland::flatland:7d78f60`, flatland._
```


## Examples

### Zero-Overhead Array abstractions

```scala
import flatland._

val array = Array(2,3,4)
array.foreachElement( elem => println(elem) )

// with the help of inlining (no macros needed), this compiles to a while loop:
val n = array.length
var i = 0

while(i < n) {
  println(array(i))
  i += 1
}

// other abstractions:
array.foreachIndex( i => println(i) )
array.foreachIndexAndElement( (i,elem) => println(s"$i: $elem") )
```

### `ArrayStack`: A simple stack with fixed size

```scala
import flatland._

val stack = ArrayStackInt.create(10)
stack.push(7)
stack.push(3)
stack.isEmpty // false
stack.isFull // false
stack.size // 2
stack.pop() // 3
stack.pop() // 7
stack.isEmpty // true
```

### `InterleavedArray`: Storing tuples in one flat array

```scala
import flatland._

val interleaved = InterleavedArrayInt.create(2)
interleaved.updatea(0,3)
interleaved.updateb(0,4)

interleaved.updatea(1,7)
interleaved.updateb(1,5)

interleaved.a(0) // 3
interleaved.b(0) // 4

interleaved.foreachTwoElements((a,b) => println(s"($a,$b)"))
// (3,4)
// (7,5)
```

### `ArraySlice`: A view to a portion of an array

```scala
import flatland._

val array = Array(2,7,5,3,4,6)
val slice = ArraySliceInt(array, start = 3, length = 2)
slice(0) // 3
slice(1) // 4
slice.toList // List(3, 4)
```

### `NestedArray`: Storing many arrays in one big flat array
```scala
import flatland._

val nested = NestedArrayInt(Array(Array(7,8,9), Array(1,2,3)))
nested(0,1) // 8
nested(1,1) // 2
nested(0).toList // List(7, 8, 9)
nested.foreachElement(1)(elem => println(elem)) // iterates over slice 1 without any additional allocation
```


### `ArraySet`: A fast integer set
An array of zeros and ones. A `1` at position `i` means the integer at index i is in the set.
This set obviously can only store the elements between `0` and `capacity - 1`.

```scala
import flatland._

val set = ArraySet.create(10)
set += 7
set += 8
set -= 7
set += 7
set.contains(7) // true
set.contains(6) // false
set.foreach(elem => println(elem)) // 7 8
```

## General High-Performance Tips
### Simple zero-overhead abstractions

Scala's `@inline` unwraps lambdas, which allows to build zero-overhead abstractions without macros:
```scala
@inline def foreachElement(self: Array[T], f: T => Unit): Unit = {
  val n = self.length
  var i = 0

  while(i < n ) {
    f(self(i))
    i += 1
  }
}
```

```scala
foreachElement(array, elem => println(elem) )
// compiles to:
val n = array.length
var i = 0

while(i < n) {
  println(array(i))
  i += 1
}
```

### Iterating twice can be faster
* When building an `Array` with unknown size, you can avoid using an `ArrayBuilder` using the following strategy: Iterate twice. First, iterate and count the number of elements you want to store in the `Array`. Then allocate an `Array` with the correct size und iterate a second time to fill it. This is often much faster, because it avoids internal re-allocations of the `ArrayBuilder`.
