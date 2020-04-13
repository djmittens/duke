package example

import java.awt.Color
import java.util.concurrent.ConcurrentLinkedDeque

import javax.swing.{Timer, WindowConstants}
import me.ngrid.duke.simulation.Simulation
import me.ngrid.duke.simulation.ca.{CellularAutomaton, WolframCode}
import me.ngrid.duke.swing

import scala.io.StdIn
import scala.util.{Random, Try}

class WolframSwing(dim: Int, code: Int) {
  val (jframe, pixels) = swing.simpleScaledBltWindow(dim, dim, 17)

  object sim {
    val DEFAULT_STATE: Array[Boolean] = {
      val init: Array[Boolean] = Array.ofDim[Boolean](dim)
      init(init.length / 2) = true
      init
    }
    val workQueue = new ConcurrentLinkedDeque[Int]()
    private[this] var game = new Simulation(
      CellularAutomaton.x1D(DEFAULT_STATE)
    )

    private[this] var wc = new WolframCode(code)

    val timer = new Timer(60, { _ =>
      tick()
      draw()
    })

    def tick(): Unit = {
      if (!workQueue.isEmpty) {
        val cmd = workQueue.poll()
        if (cmd > 0) { switchCode(cmd) }
        else if (cmd == -1) { reset() }
        else if (cmd == -2) { shakeIt() }
      }
      game.advance { ca => ca.advanceSate(wc) }
    }

    def draw(): Unit =
      game.query { f0 =>
        // shift every pixel up
        for (i <- 0 until (dim * (dim - 1))) {
          pixels(i) = pixels(i + dim)
        }
        // fetch fresh state
        for (i <- (0 until dim).reverse) {
          val x = pixels.length - 1 - i
          pixels(x) = if (f0.cells(i)) Color.GREEN.getRGB else 0
        }
      }

    def shakeIt(): Unit =
      reset(Array.tabulate(dim) { _ => Random.nextBoolean() })

    def reset(state: Array[Boolean] = DEFAULT_STATE): Unit =
      game = new Simulation(CellularAutomaton.x1D(state))

    def switchCode(c: Int): Unit = {
      wc = new WolframCode(c)
      jframe.setTitle(s"Wolfram Code ##${c}##")
    }
  }

  jframe.setTitle(s"Wolfram Code ##${code}##")
  jframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  jframe.setVisible(true)
  sim.timer.start()
}

/**
  * This is an example of a 1D automaton
  * Signals > 0 => wolfram codes
  * Signals < 0 => simulation control
  * -1 Clear           => Seed the state with a single pixel in the middle
  * -2 Shake it up     => randomly seed the state
  * -3 Use random code => use a random code
  */
object WolframSwing {
  def main(args: Array[String]): Unit = {
    val wolf = new WolframSwing(256, 105)
    while (true) {
      Try {
        val line = StdIn.readLine()
        if (line == null) System.exit(0)
        val code = Integer.parseInt(line)
        wolf.sim.workQueue.offer(code)
      }
    }
  }
}
