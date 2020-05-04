package example

import me.ngrid.duke.simulation.ca.mutable.StateBuffer
import scala.collection.mutable
import me.ngrid.duke.simulation.ca.gol.Pattern2D
import java.awt.Color
import javax.swing.Timer
import scala.collection.mutable
import java.util.concurrent.ConcurrentLinkedQueue
import me.ngrid.duke.swing
import me.ngrid.duke.simulation.Point
import java.awt.event.MouseListener
import java.awt.event.MouseEvent

class GoLEditor(
    brushDict: mutable.Map[String, GoLEditor.Buffer],
    var selectedBrush: String
) {
  val VIEWPORT_SIZE_X = 128

  val (jframe, pixels) =
    swing.simpleScaledBltWindow(VIEWPORT_SIZE_X, VIEWPORT_SIZE_X, 8)

  val canvas =
    StateBuffer.ofDim(Point._2D(VIEWPORT_SIZE_X, VIEWPORT_SIZE_X), pixels)


  object sim {
    val POINT_ZERO = Point._2D(0, 0)
    var visibleBuffers: mutable.Map[String, Color] =
      mutable.Map("gol" -> Color.MAGENTA)
    val workQueue = new ConcurrentLinkedQueue[GoLEditor.Command]()

    val timer = new Timer(100, { _ =>
      tick()
      draw()
    })

    def tick(): Unit = {
      /// Increment simulation
      var k = 1000
      while (!workQueue.isEmpty && k > 0) {
        val cmd = workQueue.poll()
        cmd match {
          case GoLEditor.Paint(x, y) =>
            // gol = gol.augment(k => brush(x, y, dim, k))
            // gol.stateBuffer.paintF[Boolean](identity)(Point._2D(x, y), brush)
            brushDict("gol").paintF[Boolean](identity)(Point._2D(x, y), brushDict(selectedBrush))
        }
        k -= 1
      }
    //   gol = gol.advanceSate(alg)
      ()
    }
    def draw(): Unit = {
      // paint onto the canvas.
      visibleBuffers.foreach {
        case (name, color) =>
          canvas.paintF(if (_) color.getRGB() else Color.BLACK.getRGB())(
            POINT_ZERO,
            brushDict(name)
          )
      }
      ()
    }
  }

  jframe.addMouseListener(listener)
  jframe.setVisible(true)
  sim.timer.start()

  object listener extends MouseListener {

    val timer = new Timer(20, { _ =>
      val pos = jframe.getMousePosition()
      if (pos != null) {
        val x: Int = (VIEWPORT_SIZE_X * (pos.getX / jframe.getWidth)).toInt
        val y: Int = (VIEWPORT_SIZE_X * (pos.getY / jframe.getHeight)).toInt
        sim.workQueue.offer(GoLEditor.Paint(x, y))
      }
    })

    override def mousePressed(e: MouseEvent): Unit = {
      if (!timer.isRunning) { timer.start() }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      if (timer.isRunning) { timer.stop() }
    }

    override def mouseClicked(x$1: MouseEvent): Unit = ()

    override def mouseEntered(x$1: MouseEvent): Unit = ()

    override def mouseExited(x$1: MouseEvent): Unit = ()


  }
}

object GoLEditor {
  type Buffer = StateBuffer[Boolean]

  sealed trait Command extends Product with Serializable
  final case class StartSim(buffer: String) extends Command
  final case class StopSim(buffer: String) extends Command
  final case class Paint(x: Int, y: Int) extends Command
  final case class SetBrush(brush: String) extends Command
  final case class DisplayBuffer(name: String, color: Color) extends Command
  final case class HideBuffer(name: String) extends Command
  final case class NewBuffer(name: String, size: (Int, Int)) extends Command

  def main(args: Array[String]): Unit = {
    new GoLEditor(mutable.Map.from(Pattern2D.apply) + ("gol" -> StateBuffer.ofDim(Point._2D(128, 128))), "beehive")
    while(true) {}
    println("Hello Wolrd")
  }
}
