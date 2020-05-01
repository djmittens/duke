package me.ngrid.duke.simulation.ca.mutable

import me.ngrid.duke.simulation.Point
import scala.collection.mutable

sealed class StateBuffer[N <: Int, State](
    size: StateBuffer.Dimensions,
    underlying: Array[State]
)(implicit d: ValueOf[N]) {

  assert(size.dimensions == d.value, "The number of dimensions presented in size must match, the dimensionality of this buffer.")
  assert(underlying.length >= size.offsets.product, "The size of the buffer must be large enough to store state of the given size")

  // Why am i having such a hard dtime visualizing this.
  def paint(offset: Point[Int], brush: StateBuffer[N, State]): Unit = {
      
  }

  def translate(from: Int, offset: Point[Int]): Int = {
    ???
  }


  private[this] def paint(
      dimsTo: Iterator[Int] = size.offsets.reverseIterator,
      offsetsFrom: Iterator[Int],
      dimsFrom: Iterator[Int],
      buffer: Array[State]
  ): Unit = {
    if (!dimsFrom.isEmpty) {
      val u = dimsFrom.next()
      val v = dimsTo.next()

      for (i <- 0 until u) {}
    }
  }

  def read(offsets: Iterator[Int]): Iterator[State] = ???
  def iterator: Iterator[State] = underlying.iterator

  private[this] def spaceWarp(x: Int, min: Int, max: Int): Int = {
    if (x < min) (max - 1) - (x - min) else x % max
  }
}

object StateBuffer {
  type Dimensions = Point[Int]
}
