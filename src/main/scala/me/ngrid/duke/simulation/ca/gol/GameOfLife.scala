package me.ngrid.duke.simulation.ca.gol

import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import java.util.concurrent.ConcurrentLinkedQueue
import me.ngrid.duke.simulation.ca.CellularAutomaton

class GameOfLife[T](size: Int, gol: CellularAutomaton[T]) {


}

object GameOfLife extends (Neighborhood[Boolean] => Boolean) {

  // val INITIAL_STATE: Array[Boolean] = {
  //   Array.ofDim[Boolean](dim * dim)
  // }
  override def apply(v1: Neighborhood[Boolean]): Boolean = {
    assert(v1.length == 9, "These rules are only for 2D automata, which have neighborhood size of 9")

    // the middle element will always be self.
    val self = v1(v1.length / 2)
    val popCount = v1.foldLeft(0)((acc, c) => if(c) acc + 1 else acc)
    if(self) {
      // alive
      val pop = popCount - 1 // minus our selves
      // 1 . Any live cell with fewer than two live neighbours dies, as if by underpopulation.
      if(pop < 2) {
        false
      }
      // 2 . Any live cell with two or three live neighbours lives on to the next generation.
      else if(pop == 2 || pop == 3) {
        true
      }
      // 3 . Any live cell with more than three live neighbours dies, as if by overpopulation.
      else {
        false
      }
    } else {
      //dead
      // 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      popCount == 3
    }
  }
}
