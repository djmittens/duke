package me.ngrid.duke.simulation.ca.mutable

import me.ngrid.duke.simulation.Point
import scala.collection.mutable
import scala.annotation.tailrec
import scala.reflect.ClassTag
import me.ngrid.duke.simulation.NDimensional

sealed class StateBuffer[State](
    val size: StateBuffer.Dimensions,
    private val underlying: Array[State]
) {
  assert(size.dimensions > 0)
  assert(
    underlying.length >= size.offsets.product,
    "The size of the buffer must be large enough to store state of the given size"
  )

  val dimensions = size.dimensions
  private val prods = size.offsets.tail.foldRight(1 :: Nil) { (i, acc) =>
    (i * acc.head) :: acc
  }

  // Why am i having such a hard dtime visualizing this.
  def paintF[T](
      f: T => State
  )(offset: NDimensional[Int], brush: StateBuffer[T]): Unit = {
    assert(offset.dimensions == size.dimensions)

    // println(prods)
    paint(
      offset.offsets.toList,
      size.offsets.toList,
      brush.size.offsets.toList,
      brush.prods,
      prods,
      0,
      0
    )

    def paint(
        off: List[Int],
        dimsC: List[Int],
        dimsB: List[Int],
        prodsB: List[Int],
        prodsC: List[Int],
        from: Int,
        to: Int
    ): Unit = {
      off match {
        case Nil =>
          if (from >= 0 && from < brush.underlying.length && to >= 0 && to < underlying.length) {
            // if(true) {
            underlying(to) = f(brush.underlying(from))
          }
        case o :: _ =>
          val pB = prodsB.head
          val pC = prodsC.head

          (0 until dimsB.head).foreach { i =>
            val k = i + o
            if(k >= 0 && k < dimsC.head) {
              paint(
                off.tail,
                dimsC.tail,
                dimsB.tail,
                prodsB.tail,
                prodsC.tail,
                from + (i * pB),
                to + (k * pC)
              )
            }
          }
      }
    }
  }

  def foreach(f: State => Unit): Unit = underlying.foreach(f)
  def iterator: Iterator[State] = underlying.iterator

  private[this] def spaceWarp(x: Int, min: Int, max: Int): Int = {
    if (x < min) (max - 1) - (x - min) else x % max
  }
}

object StateBuffer {
  type Dimensions = NDimensional[Int]
  def ofDim[State: ClassTag](dimensions: Dimensions): StateBuffer[State] =
    ofDim(dimensions, Array.ofDim[State](dimensions.offsets.product))

  def ofDim[State](
      dimensions: Dimensions,
      buffer: Array[State]
  ): StateBuffer[State] =
    new StateBuffer[State](dimensions, buffer)
}
