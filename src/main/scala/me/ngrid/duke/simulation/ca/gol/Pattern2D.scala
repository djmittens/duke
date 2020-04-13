package me.ngrid.duke.simulation.ca.gol

final case class Pattern2D[D <: Int](underlying: Array[Boolean])(implicit d: ValueOf[D]) {
  assert(underlying.length == (d.value * d.value))

  def apply(x: Int, y: Int, stateDimLength: Int, state: Array[Boolean]): Array[Boolean] = {
    assert(state.length > underlying.length)
    val res = state.clone()
    for(k <- 0 until d.value) {
      for (i <- 0 until d.value) {
        res(loop(k + y, stateDimLength) * stateDimLength  + loop(i + x, stateDimLength)) = underlying(k * d.value + i)
      }
    }
    res
  }

  def loop(x: Int, len: Int): Int = if(x < 0) len - 1 else x % len
}

object Pattern2D {
  //Still lifes
  val block = new Pattern2D[2](Array(
    true, true,
    true, true,
  ))

  val tub = new Pattern2D[3](Array(
    false, true, false,
    true, false, true,
    false, true, false,
  ))

  val beehive = new Pattern2D[4](Array(
    false, true, true, false,
    true, false,false, true,
    false, true, true, false,
    false, false, false, false,
  ))

  /// Oscillators
  val blinker = new Pattern2D[3](Array(
    false, true, false,
    false, true, false,
    false, true, false,
  ))

  val toad = new Pattern2D[4](Array(
    false, false, false, false,
    false, true, true, true,
    true, true, true, false,
    false, false, false, false,
  ))

  val beacon = new Pattern2D[4](Array(
    true, true, false, false,
    true, true, false, false,
    false, false, true, true,
    false, false, true, true,
  ))


  /// Spaceships -> Conway actually wanted to call this an ant, he regrets it
  val glider = new Pattern2D[3](Array(
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

