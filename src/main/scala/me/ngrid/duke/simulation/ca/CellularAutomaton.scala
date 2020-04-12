package me.ngrid.duke.simulation.ca

import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import me.ngrid.duke.util.{NArray, Show}

import scala.reflect.ClassTag

final class CellularAutomaton[D <: Int, Cell: ClassTag] private (private val cells: Array[Cell], dim: Int)  (implicit s: ValueOf[D]) {
  val numCells = 64
  private[this] val hoods = {
    Array.tabulate(cells.length){ i =>
      neighbors(i, 0, cells.length, dim)
    }
  }

  private[this] def neighbors(v: Int, start: Int,  len: Int, dim: Int): Array[Int]= {
    val blockSize = len / numCells
    if (dim == 1) {
      Array(loopAround(v-1, numCells), v, loopAround(v+1, numCells))
    } else {
      // cells ( xDIM * size ^ DIM  + xDIM-1 * size ^ DIM-1 .... + x0)
      // v = (xDIM * size ^ DIM) + (xDIM-1 * size ^ DIM-1 .... + x0)
      val block = v / blockSize
      val offset = v % blockSize

      val left = neighbors(offset, start + (loopAround(block - 1, numCells) * blockSize), blockSize, dim - 1)
      val self = neighbors(offset, start + (block * blockSize), blockSize, dim - 1)
      val right = neighbors(offset, start + (loopAround(block + 1, numCells) * blockSize), blockSize, dim - 1)

      left ++ self ++ right
    }
  }

  def update(f: Neighborhood[D, Cell] => Cell): CellularAutomaton[D, Cell] = {
    val next = Array.ofDim[Cell](cells.length)

    for(d <- cells.indices) {next(d) = f(neighbors(d))}

    new CellularAutomaton(next, dim)
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
    new CellularAutomaton(init, 1)
  }

  implicit def show[D <: Int, Cell](implicit s: Show[Cell]): Show[CellularAutomaton[D, Cell]] = (v: CellularAutomaton[D, Cell]) => {
    val stringBuilder = new StringBuilder
    v.foreach(x => stringBuilder.append(s.apply(x)))
    stringBuilder.toString()
  }

  type Neighborhood[Nit <: Int, State] = NArray[Nit, State]
  object Neighborhood {}
}
