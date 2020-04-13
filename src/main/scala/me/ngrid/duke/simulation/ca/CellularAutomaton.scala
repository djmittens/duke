package me.ngrid.duke.simulation.ca

import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import me.ngrid.duke.util.{NArray, Show}

import scala.collection.mutable
import scala.reflect.ClassTag

final class CellularAutomaton[D <: Int, Cell: ClassTag] private (
    val cells: Array[Cell],
    dimLength: Int,
    dim: Int,
    hoods: Array[Array[Int]]
)(implicit s: ValueOf[D]) {

  def advanceSate(
      f: Neighborhood[D, Cell] => Cell
  ): CellularAutomaton[D, Cell] = {
    val next = Array.ofDim[Cell](cells.length)

    for (d <- cells.indices) { next(d) = f(neighbors(d)) }

    new CellularAutomaton(next, dimLength, dim, hoods)
  }

  def augment[T : ClassTag](f: Array[Cell] => Array[T]): CellularAutomaton[D, T] = {
    val res = f(cells)
    assert(
      res.length == cells.length,
      "Augmenting, the automaton, must not change its dimension!, please re-make it instead."
    )
    new CellularAutomaton(res, dimLength, dim, hoods)
  }

  def neighbors(v: Int): Neighborhood[D, Cell] =
    NArray[D, Cell](hoods(v).map(cells.apply))

  def foreach(f: Cell => Unit): Unit = cells.foreach(f)
}

object CellularAutomaton {

  def x1D[Cell: ClassTag](init: Array[Cell]): CellularAutomaton[3, Cell] =
    new CellularAutomaton(
      init,
      init.length,
      1,
      calculateNeighborhoods(init.length, 1, init.length)
    )

  def x2D[Cell: ClassTag](
      size: Int,
      init: Array[Cell]
  ): CellularAutomaton[9, Cell] =
    new CellularAutomaton(
      init,
      size,
      2,
      calculateNeighborhoods(size, 2, init.length)
    )

  implicit def show[D <: Int, Cell](
      implicit s: Show[Cell]
  ): Show[CellularAutomaton[D, Cell]] = (v: CellularAutomaton[D, Cell]) => {
    val stringBuilder = new StringBuilder
    v.foreach(x => stringBuilder.append(s.apply(x)))
    stringBuilder.toString()
  }

  private[this] def calculateNeighborhoods(
      dimLength: Int,
      dimensions: Int,
      numCells: Int
  ) = {
    def neighbors(v: Int, start: Int, len: Int, dim: Int): Array[Int] = {
      val blockSize = len / dimLength
      if (dim == 1) {
        Array(loopAround(v - 1, dimLength), v, loopAround(v + 1, dimLength))
      } else {
        // cells ( xDIM * size ^ DIM  + xDIM-1 * size ^ DIM-1 .... + x0)
        // v = (xDIM * size ^ DIM) + (xDIM-1 * size ^ DIM-1 .... + x0)
        val block = v / blockSize
        val offset = v % blockSize

        val leftBlock = loopAround(block - 1, dimLength)
        val rightBlock = loopAround(block + 1, dimLength)

        val left =
          neighbors(offset, start + (leftBlock * blockSize), blockSize, dim - 1)
        val self =
          neighbors(offset, start + (block * blockSize), blockSize, dim - 1)
        val right = neighbors(
          offset,
          start + (rightBlock * blockSize),
          blockSize,
          dim - 1
        )

        val buf = mutable.ArrayBuilder.make[Int]
        buf.addAll(left.map(_ + (leftBlock * blockSize)))
        buf.addAll(self.map(_ + (block * blockSize)))
        buf.addAll(right.map(_ + (rightBlock * blockSize)))
        buf.result()
      }
    }

    Array.tabulate(numCells) { i => neighbors(i, 0, numCells, dimensions) }
  }

  private[this] def loopAround(i: Int, len: Int): Int = {
    if (i < 0) len - 1 else i % len
  }

  type Neighborhood[Nit <: Int, State] = NArray[Nit, State]
  object Neighborhood {}
}
