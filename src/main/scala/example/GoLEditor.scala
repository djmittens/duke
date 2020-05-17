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
import java.awt.event.MouseWheelListener
import java.awt.event.MouseWheelEvent
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentListener
import java.awt.event.ComponentEvent
import me.ngrid.duke.simulation.NDimensional
import scala.collection.View
import java.awt.Canvas
import me.ngrid.duke.swing.PixelBltCanvas

class GoLEditor(
    brushDict: mutable.Map[String, GoLEditor.Buffer],
    var selectedBrush: String
) {

  val (jframe, bltCanvas) =
    swing.simpleScaledBltWindow(256, 256, 8)

  val viewport = GoLEditor.GolViewport(bltCanvas)(256, 256)


  object sim {
    val ZOOM_MULTIPLIER = 10
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
            val offX = (x * viewport.sizeX).toInt - viewportOffset.x
            val offY = (y * viewport.sizeY).toInt - viewportOffset.y
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
              (x * viewport.sizeX).toInt,
              (y * viewport.sizeY).toInt
            )
          case GoLEditor.Viewport.Pan(x, y) =>
            val p = Point._2D(
              (x * viewport.sizeX).toInt,
              (y * viewport.sizeY).toInt
            )
            val d = p - pin
            viewportOffset = viewportOffset + d
            pin = p

          case GoLEditor.Zoom(amnt) =>
            viewport.zoom(amnt * ZOOM_MULTIPLIER)
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
      bltCanvas.clear(Color.GRAY)
      // paint onto the canvas.
      visibleBuffers.foreach {
        case (name, color) =>
          viewport.paintF(if (_) color.getRGB() else Color.BLACK.getRGB())(
            viewportOffset,
            brushDict(name)
          )
      }
      ()
    }
  }

  bltCanvas.addMouseListener(listener)
  bltCanvas.addMouseWheelListener(listener)
  jframe.setVisible(true)
  sim.timer.start()

  object listener
      extends MouseListener
      with MouseWheelListener {

    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      sim.workQueue.offer(GoLEditor.Zoom(e.getWheelRotation()))
    }

    def normalizedMousePosition: (Double, Double) = {
      val pos = bltCanvas.getMousePosition()
      if (pos != null) {
        import pos._
        x.toDouble / bltCanvas.getWidth() -> y.toDouble / bltCanvas.getHeight()
      } else 0d -> 0d
    }

    def newEventEmitter(
        delay: Int
    )(newEvent: (Double, Double) => GoLEditor.Command): Timer = {
      val f = newEvent.tupled
      new Timer(delay, { _ =>
        val pos = bltCanvas.getMousePosition()
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

  type Viewport = Point._2D[Int]
  object Viewport {
    final case class Pin(x: Double, y: Double) extends Command
    final case class Pan(x: Double, y: Double) extends Command
  }

  final case class Zoom(amount: Int) extends Command

  final case class SetBrush(brush: String) extends Command
  final case class DisplayBuffer(name: String, color: Color) extends Command
  final case class HideBuffer(name: String) extends Command
  final case class NewBuffer(name: String, size: (Int, Int)) extends Command


  final case class GolViewport(val awtCanvas: PixelBltCanvas )(var sizeX: Int = awtCanvas.getWidth(), var sizeY: Int = awtCanvas.getHeight()) {
    var buffer: StateBuffer[Int] = _
    // resize(this.sizeX, this.sizeY)

    awtCanvas.addComponentListener(new ComponentListener {

      override def componentResized(x$1: ComponentEvent): Unit = {
        resize(sizeX, sizeY)
      }

      override def componentMoved(x$1: ComponentEvent): Unit = ()

      override def componentShown(x$1: ComponentEvent): Unit = ()

      override def componentHidden(x$1: ComponentEvent): Unit = ()
    })

    def alignAspectRatio(x: Int, y: Int): (Int, Int) = {
      val w = awtCanvas.getWidth()
      val h = awtCanvas.getHeight()
      if (w > h) {
        x -> ((h.toDouble / w) * x).toInt
      } else {
        ((w.toDouble / h) * y).toInt -> y
      }
    }

    def resize(x: Int, y: Int): Unit = {
      val (ax, ay) = alignAspectRatio(x, y)
      this.sizeX = ax
      this.sizeY = ay
      awtCanvas.resizeBuffer(ax, ay)
      buffer = StateBuffer.ofDim(Point._2D(ax, ay), awtCanvas.pixels)
    }

    def zoom(pixels: Int): Unit = {
      resize(sizeX + pixels, sizeY + pixels)
    }

    def paintF[T](f: T => Int)(offset: Point._2D[Int], state: StateBuffer[T]): Unit = {
      buffer.paintF(f)(offset, state)
    }

  }

  def main(args: Array[String]): Unit = {

    val defaultBuffers =
      ("gol" -> StateBuffer.ofDim[Boolean](Point._2D(128, 128))) :: Nil
    new GoLEditor(
      mutable.Map.from(Pattern2D.apply).concat(defaultBuffers),
      "beehive"
    )
    while (true) {
      Thread.sleep(1000)
    }
    println("Hello Wolrd")
  }
}
