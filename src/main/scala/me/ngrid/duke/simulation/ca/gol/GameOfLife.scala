package me.ngrid.duke.simulation.ca.gol

import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import java.util.concurrent.ConcurrentLinkedQueue
import me.ngrid.duke.simulation.ca.CellularAutomaton

class GameOfLife[N <: Int, T](dim: Int, gol: CellularAutomaton[N, T]) {
  val workQueue = new ConcurrentLinkedQueue[GameOfLife.Command]()

  val INITIAL_STATE: Array[Boolean] = {
    Array.ofDim[Boolean](dim * dim)
  }

  // private[this] var gol = 
  //   CellularAutomaton.x2D(dim, INITIAL_STATE)

  // def tick(): Unit = {
  //   var k = 1000
  //   while (!workQueue.isEmpty && k > 0) {
  //     val cmd = workQueue.poll()
  //     cmd match {
  //       case GameOfLife.Paint(brush ) =>
  //         gol = gol.augment(k => brush(x, y, dim, k))
  //     }
  //     k -= 1
  //   }
  //   gol = gol.advanceSate(GameOfLife)
  // }

}

object GameOfLife extends (Neighborhood[9, Boolean] => Boolean) {
  sealed trait Command extends Product with Serializable
  final case class Paint[N <: Int](x: Int, y: Int, brush: Brush[N]) extends Command


  // val INITIAL_STATE: Array[Boolean] = {
  //   Array.ofDim[Boolean](dim * dim)
  // }
  override def apply(v1: Neighborhood[9, Boolean]): Boolean = {

    val self = v1.underlying(9 / 2)
    val popCount = v1.underlying.foldLeft(0)((acc, c) => if(c) acc + 1 else acc)
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
