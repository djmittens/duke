package example

import me.ngrid.duke.simulation.Simulation
import me.ngrid.duke.simulation.ca.CellularAutomaton
import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import me.ngrid.duke.util.Show

object WolframCodeRun {
  implicit val bshow: Show[Boolean] = (b: Boolean) => if(b) "*" else " "

  def main(args: Array[String]): Unit = {
    val init = Array.ofDim[Boolean](64)
    // seed this mofo
    init(init.length / 2) = true


    val game = new Simulation(CellularAutomaton.x1D(init))
    val wc = new WolframCode(182)
    for (_ <- 0 until 32) {
      game.advance {ca =>
        ca.update(wc)
      }

      game.println()
    }
  }
}

class WolframCode(c: Int) extends (Neighborhood[3, Boolean] => Boolean )  {
  override def apply(v1: Neighborhood[3, Boolean]): Boolean = {
    v1.underlying match {
      case Array(left, self, right) =>
        advance(tint(left, 2) | tint(self, 1) | tint(right, 0))
    }
  }

  def advance(i: Int):Boolean = (c & (1 << i)) > 0

  // translate int
  def tint(s: Boolean, k: Int): Int = if(s) 1 << k else 0
}
