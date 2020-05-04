package me.ngrid.duke.simulation.ca

import me.ngrid.duke.simulation.ca.CellularAutomaton.Neighborhood

class WolframCode(val code: Int) extends (Neighborhood[Boolean] => Boolean )  {
  override def apply(v1: Neighborhood[Boolean]): Boolean = {
    v1 match {
      case Array(left, self, right) =>
        advance(tint(left, 2) | tint(self, 1) | tint(right, 0))
    }
  }

  def advance(i: Int):Boolean = (code & (1 << i)) > 0

  // translate int
  def tint(s: Boolean, k: Int): Int = if(s) 1 << k else 0
}
