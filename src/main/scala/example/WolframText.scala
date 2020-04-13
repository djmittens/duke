package example

import me.ngrid.duke.simulation.Simulation
import me.ngrid.duke.simulation.ca.{CellularAutomaton, WolframCode}
import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood
import me.ngrid.duke.util.Show

object WolframText {
  implicit val bshow: Show[Boolean] = (b: Boolean) => if(b) "*" else " "

  def main(args: Array[String]): Unit = {
    val init = Array.ofDim[Boolean](64)
    // seed this mofo
    init(init.length / 2) = true


    val game = new Simulation(CellularAutomaton.x1D(init))
    val wc = new WolframCode(182)
    for (_ <- 0 until 32) {
      game.advance {ca =>
        ca.advanceSate(wc)
      }

      game.println()
    }
  }
}
