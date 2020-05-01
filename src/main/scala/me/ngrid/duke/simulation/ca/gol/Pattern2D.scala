package me.ngrid.duke.simulation.ca.gol

import me.ngrid.duke.simulation.Point

final case class Pattern2D(size: Point._2D[Int], underlying: Array[Boolean]) extends Brush[Boolean]{
  // assert(offsets.length == (size.x * size.y))

  override val offsets = underlying
  override val dimensions = 2

  def apply(x: Int, y: Int, stateDimLength: Int, state: Array[Boolean]): Array[Boolean] = {
    assert(state.length > offsets.length)
    val res = state.clone()
    for(k <- 0 until size.y) {
      for (i <- 0 until size.x) {
        res(loop(k + y, stateDimLength) * stateDimLength  + loop(i + x, stateDimLength)) = offsets(k * size.y + i)
      }
    }
    res
  }

  def loop(x: Int, len: Int): Int = if(x < 0) len - 1 else x % len
}

object Pattern2D {
  private[this] val p = Point._2D.apply[Int] _

  //Still lifes
  val block = new Pattern2D(p(2,2), Array(
    true, true,
    true, true,
  ))

  val tub = new Pattern2D(p(3,3), Array(
    false, true, false,
    true, false, true,
    false, true, false,
  ))

  val beehive = new Pattern2D(p(4,3), Array(
    false, true, true, false,
    true, false,false, true,
    false, true, true, false,
  ))

  /// Oscillators
  val blinker = new Pattern2D(p(1, 3), Array(
    true,
    true,
    true,
  ))

  val toad = new Pattern2D(p(4, 2), Array(
    false, true, true, true,
    true, true, true, false,
  ))

  val beacon = new Pattern2D(p(4,4), Array(
    true, true, false, false,
    true, true, false, false,
    false, false, true, true,
    false, false, true, true,
  ))


  /// Spaceships -> Conway actually wanted to call this an ant, he regrets it
  val glider = new Pattern2D(p(3,3), Array(
    false, false, true,
    true, false, true,
    false, true, true,
  ))

  val apply = Map(
    "block" -> block, "box" -> block, // man i keep making this mistake but im gonna stick with it.
    "tub" -> tub,
    "beehive" -> beehive,
    "blinker" -> blinker,
    "toad" -> toad,
    "beacon" -> beacon,
    "glider" -> glider,
  )
}

