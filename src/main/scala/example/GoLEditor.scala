package example

import me.ngrid.duke.simulation.ca.mutable.StateBuffer
import scala.collection.mutable
import me.ngrid.duke.simulation.ca.gol.Pattern2D
import java.awt.Color
import javax.swing.Timer
import scala.collection.mutable
import scala.collection.immutable
import java.util.concurrent.ConcurrentLinkedQueue
import me.ngrid.duke.swing
import me.ngrid.duke.simulation.Point
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.MouseInfo

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
    val POINT_ZERO = Point._2D[Int](0, 0)
    var visibleBuffers: mutable.Map[String, Color] =
      mutable.Map("gol" -> Color.MAGENTA)
    val workQueue = new ConcurrentLinkedQueue[GoLEditor.Command]()

    var viewportOffset = POINT_ZERO
    var pin = POINT_ZERO

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
            val offX = (x * VIEWPORT_SIZE_X).toInt
            val offY = (y * VIEWPORT_SIZE_X).toInt
            // gol = gol.augment(k => brush(x, y, dim, k))
            // gol.stateBuffer.paintF[Boolean](identity)(Point._2D(x, y), brush)
            val b = brushDict(selectedBrush)
            val Seq(bY, bX) = b.size.offsets
            brushDict("gol").paintF[Boolean](identity)(
              Point._2D(offX - (bX / 2), offY - (bY / 2)),
              b
            )
          case GoLEditor.Viewport.Pin(x, y) =>
            pin = Point._2D(
              (x * VIEWPORT_SIZE_X).toInt,
              (y * VIEWPORT_SIZE_X).toInt
            )
          case GoLEditor.Viewport.Pan(x, y) =>
            val p = Point._2D(
              (x * VIEWPORT_SIZE_X).toInt,
              (y * VIEWPORT_SIZE_X).toInt
            )
            val d = p - pin
            viewportOffset = viewportOffset + d
            pin = p
          case other =>
            System.err.println(
              s"Editor command $other is not supported at the moment"
            )
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
            viewportOffset,
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
    val canvas = jframe.getComponents().head

    def normalizedMousePosition: (Double, Double) = {
      val pos = canvas.getMousePosition()
      if (pos != null) {
        import pos._
        x.toDouble / canvas.getWidth() -> y.toDouble / canvas.getHeight()
      } else 0d -> 0d
    }

    def newEventEmitter(
        delay: Long
    )(newEvent: (Double, Double) => GoLEditor.Command): Timer = {
      val f = newEvent.tupled
      new Timer(20, { _ =>
        val pos = canvas.getMousePosition()
        if (pos != null) {
          // println(s"${pos.getX} -> ${pos.getY}")
          // This assumes that the only component is the canvas.
          sim.workQueue.offer(f(normalizedMousePosition))
        }
      })
    }

    // val timers = mutable.Map.empty[Int, Timer]
    val timers = {
      mutable.Map[Int, Timer](
        MouseEvent.BUTTON1 -> newEventEmitter(4)(GoLEditor.Paint),
        MouseEvent.BUTTON2 -> newEventEmitter(4)(GoLEditor.Viewport.Pan)
      )
    }

    def isTimerRunning(button: Int): Boolean =
      timers.contains(button) && timers(button).isRunning()

    override def mousePressed(e: MouseEvent): Unit = {
      val button = e.getButton()

      button match {
        case MouseEvent.BUTTON2 =>
          sim.workQueue.offer(
            GoLEditor.Viewport.Pin.tupled(normalizedMousePosition)
          )
        case _ => ()
      }

      // begin processing timers
      if (timers.contains(button) && !isTimerRunning(button)) {
        timers(button).start()
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      val button = e.getButton()
      if (timers.contains(button) && isTimerRunning(button))
        timers(button).stop()
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
  // 0 to 1
  final case class Paint(x: Double, y: Double) extends Command

  object Viewport {
    final case class Pin(x: Double, y: Double) extends Command
    final case class Pan(x: Double, y: Double) extends Command
  }

  final case object ZoomIn extends Command
  final case object ZoomOut extends Command

  final case class SetBrush(brush: String) extends Command
  final case class DisplayBuffer(name: String, color: Color) extends Command
  final case class HideBuffer(name: String) extends Command
  final case class NewBuffer(name: String, size: (Int, Int)) extends Command

  def main(args: Array[String]): Unit = {

    val defaultBuffers =
      ("gol" -> StateBuffer.ofDim[Boolean](Point._2D(128, 128))) :: Nil
    new GoLEditor(
      mutable.Map.from(Pattern2D.apply).concat(defaultBuffers),
      "beehive"
    )
    while (true) {}
    println("Hello Wolrd")
  }
}
