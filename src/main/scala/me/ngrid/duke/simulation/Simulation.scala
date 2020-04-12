package me.ngrid.duke.simulation

import me.ngrid.duke.util.Show


class Simulation[State](initial: State) {

  private[this] var state: State = initial

  def println()(implicit s: Show[State]): Unit = {
    System.out.println(s.apply(state))
  }


  def advance(f: State => State): Unit = {
    state = f(state)
  }

  def query[T](f: State => T): T = {
    f(state)
  }
}
