package example

import java.awt.Color
import java.awt.event.{
  KeyEvent,
  KeyListener,
  MouseEvent,
  MouseListener,
  MouseMotionListener
}
import java.util.concurrent.ConcurrentLinkedQueue

import javax.swing.Timer
import me.ngrid.duke.simulation.ca.CellularAutomaton
import me.ngrid.duke.simulation.ca.gol.{GameOfLife, Pattern2D}
import me.ngrid.duke.simulation.Point
import me.ngrid.duke.swing

import scala.io.StdIn
import scala.util.Try
import me.ngrid.duke.simulation.ca.mutable.StateBuffer

class GameOfLifeSwing(dim: Int) {
  val (jframe, canvas) = swing.simpleScaledBltWindow(dim, dim, 17)

  var brush: StateBuffer[Boolean] = Pattern2D.beacon

  object sim {
    sealed trait Command extends Product with Serializable
    final case class Paint(x: Int, y: Int) extends Command
    final case class SetBrush(brush: StateBuffer[Boolean]) extends Command

    val workQueue = new ConcurrentLinkedQueue[Command]()

    val INITIAL_STATE: Array[Boolean] = {
      Array.ofDim[Boolean](dim * dim)
    }

    val timer = new Timer(100, { _ =>
      tick()
      draw()
    })

    private[this] var gol =
      CellularAutomaton.x2D(dim, INITIAL_STATE)
    private[this] val alg = GameOfLife

    def draw(): Unit = {
      val green = Color.GREEN.getRGB
      for (i <- gol.cells.indices) {
        canvas.pixels(i) = if (gol.cells(i)) green else 0
      }
    }

    def tick(): Unit = {
      var k = 1000
      while (!workQueue.isEmpty && k > 0) {
        val cmd = workQueue.poll()
        cmd match {
          case Paint(x, y) =>
            // gol = gol.augment(k => brush(x, y, dim, k))
            gol.stateBuffer.paintF[Boolean](identity)(Point._2D(x, y), brush)
          case SetBrush(b) =>
            brush = b
        }
        k -= 1
      }
      gol = gol.advanceSate(alg)
    }
  }

  jframe.addMouseListener(listener)
//  jframe.addKeyListener(listener)
  jframe.pack()
  jframe.setVisible(true)
  jframe.requestFocus()
  sim.timer.start()

  object listener extends MouseListener {

    val timer = new Timer(20, { _ =>
      val pos = jframe.getMousePosition()
      if (pos != null) {
        val x: Int = (dim * (pos.getX / jframe.getWidth)).toInt
        val y: Int = (dim * (pos.getY / jframe.getHeight)).toInt
        sim.workQueue.offer(sim.Paint(x, y))
      }
    })

    override def mousePressed(e: MouseEvent): Unit = {
      if (!timer.isRunning) { timer.start() }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      if (timer.isRunning) { timer.stop() }
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}

    override def mouseClicked(e: MouseEvent): Unit = {}
  }
}

object GameOfLifeSwing {
  private val set_brush = """b (.+)""".r
  private val paint = """p ([0-9]+) ([0-9]+)""".r

  def main(args: Array[String]): Unit = {
//    val game = new GameOfLifeSwing(3840 / 6)
    // val game = new GameOfLifeSwing(1024)
    val game = new GameOfLifeSwing(512)
    while (true) Try {
      val input = StdIn.readLine()
      if (input == null) System.exit(0)
      input match {
        case set_brush(x) =>
          game.sim.workQueue.offer(
            game.sim.SetBrush(Pattern2D(x))
          )
        case paint(x, y) =>
          game.sim.workQueue.offer(
            game.sim.Paint(Integer.parseInt(x), Integer.parseInt(y))
          )
      }
    }.failed.foreach(e => e.printStackTrace())
  }
}
