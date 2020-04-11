package example



class GameOfLife1D {

  type State = Boolean
  type GameState = Array[State]

  final case class Neighborhood(neighbors: Array[State])
  object Neighborhood2 {
    def unapply(n: Neighborhood): Option[(State, State, State)] = {
      n.neighbors match {
        case Array(l, s, r) => Some((l, s, r))
        case _ => None
      }
    }
  }

  type  NextState = (Neighborhood) => State

  val size = 64 // 16 x 16 grid?
  var state: GameState = Array.ofDim(size)
//  state(0) = true
  state(state.length / 2 ) = true
//  state(state.length / 2 + 1) = true
//  state(state.length / 2 + 2) = true

  def println(): Unit = {
    state.foreach {
      if(_) print("*") else print("_")
    }
    print("\n")
  }

  def neighbors(i: Int): Neighborhood = {
    val right = state((i + 1) % state.length)
    val self = state(i)
    val left = state(if(i == 0) state.length - 1 else i - 1)
    Neighborhood(Array(left, self, right))
  }

  def advance(f: NextState): Unit = {
    val next: GameState = Array.ofDim(size)
    for (i <- state.indices) {
      next(i) = f(neighbors(i))
    }
    state = next
  }

  val x182: NextState = {
    case Neighborhood2(true, true, true) =>  true
    case Neighborhood2(true, true, false) =>  false
    case Neighborhood2(true, false, true) =>  true
    case Neighborhood2(true, false, false) =>  true
    case Neighborhood2(false, true, true) =>  false
    case Neighborhood2(false, true, false) =>  true
    case Neighborhood2(false, false, true) =>  true
    case Neighborhood2(false, false, false) =>  false
  }

  val x30: NextState = new WolframCode(30)
  val x222: NextState = new WolframCode(222)
  val x102: NextState = new WolframCode(102)

  def wolframCode(c: Int) = new WolframCode(c)
  class WolframCode(c: Int) extends NextState {
    override def apply(v1: Neighborhood): State = advance(dist(v1))
    def dist(n: Neighborhood): Int = {
      def tint(s: Boolean, k: Int) = if(s) 1 << k else 0
      val Neighborhood2(left, self, right) = n
      tint(left, 2) | tint(self, 1) | tint(right, 0)
    }

    def advance(i: Int):Boolean = (c & (1 << i)) > 0
  }
}

object GameOfLifeRun {
  def main(args: Array[String]): Unit = {
    val game = new GameOfLife1D()
    for (i <- 0 until game.size) {
      game.advance(game.wolframCode(45))
      game.println()
    }

  }

}
