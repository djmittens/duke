package me.ngrid.duke.simulation.ca

import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import me.ngrid.duke.util.{NArray, Show}

import scala.collection.mutable
import scala.reflect.ClassTag

final class CellularAutomaton[D <: Int, Cell: ClassTag] private(val cells: Array[Cell], length: Int, dim: Int)(implicit s: ValueOf[D]) {
  private[this] val hoods = {
    Array.tabulate(cells.length){ i =>
      neighbors(i, 0, cells.length, dim)
    }
  }

  private[this] def neighbors(v: Int, start: Int,  len: Int, dim: Int): Array[Int]= {
    val blockSize = len / length
    if (dim == 1) {
      Array(loopAround(v-1, length), v, loopAround(v+1, length))
    } else {
      // cells ( xDIM * size ^ DIM  + xDIM-1 * size ^ DIM-1 .... + x0)
      // v = (xDIM * size ^ DIM) + (xDIM-1 * size ^ DIM-1 .... + x0)
      val block = v / blockSize
      val offset = v % blockSize

      val left = neighbors(offset, start + (loopAround(block - 1, length) * blockSize), blockSize, dim - 1)
      val self = neighbors(offset, start + (block * blockSize), blockSize, dim - 1)
      val right = neighbors(offset, start + (loopAround(block + 1, length) * blockSize), blockSize, dim - 1)

      val buf = mutable.ArrayBuilder.make[Int]
      buf.addAll(left)
      buf.addAll(self)
      buf.addAll(right)
      buf.result()
    }
  }

  def update(f: Neighborhood[D, Cell] => Cell): CellularAutomaton[D, Cell] = {
    val next = Array.ofDim[Cell](cells.length)

    for(d <- cells.indices) {next(d) = f(neighbors(d))}

    new CellularAutomaton(next, length, dim)
  }

  def neighbors(v: Int):Neighborhood[D, Cell] =
    NArray[D, Cell](hoods(v).map(cells.apply))

  private[this] def loopAround(i: Int, len: Int): Int = {
    if(i < 0) len - 1 else i % len
  }

  def foreach(f: Cell => Unit): Unit = cells.foreach(f)
}

object CellularAutomaton {

  def x1D[Cell : ClassTag](init: Array[Cell]): CellularAutomaton[3, Cell] = {
    new CellularAutomaton(init, init.length, 1)
  }

  implicit def show[D <: Int, Cell](implicit s: Show[Cell]): Show[CellularAutomaton[D, Cell]] = (v: CellularAutomaton[D, Cell]) => {
    val stringBuilder = new StringBuilder
    v.foreach(x => stringBuilder.append(s.apply(x)))
    stringBuilder.toString()
  }

  type Neighborhood[Nit <: Int, State] = NArray[Nit, State]
  object Neighborhood {}
}
