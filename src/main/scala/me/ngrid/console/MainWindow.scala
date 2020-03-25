package me.ngrid.console

import javax.swing.JFrame
import java.awt.Canvas
import java.awt.Dimension
import java.awt.image.BufferStrategy
import java.awt.Color
import java.awt.event.WindowEvent
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Promise
import scala.concurrent.Future
import java.awt.Image

trait MainWindow[F[_]] {
  type Buffer

  def start()
  def paint(f: Buffer => Unit): F[Unit]
  def stop()
  def close()
}

class SwingWindow() extends MainWindow[Future] {

  private[this] var running = true

  private[this] val workQueue =
    new ConcurrentLinkedQueue[(Buffer => Unit, Promise[Unit])]()

  private[this] var thread = new Thread("Renderman") {
    override def run(): Unit = {
      var strat: BufferStrategy = null
      while (strat == null) strat = canvas.getBufferStrategy()

      def draw() = {
        val graphics = strat.getDrawGraphics()
        val (f, p) = workQueue.poll()
        graphics.setColor(Color.RED)
        graphics.fillRect(0, 0, 100, 100)
        strat.show()
        graphics.dispose()
        p.success(())
      }

      try {
        do {
          if (!workQueue.isEmpty()) draw()
          Thread.sleep(1000)
        } while (running)

      } catch {
        case ex => println(ex)
      }
    }
  }

  private val canvas: Canvas = new Canvas {
    override def getPreferredSize(): Dimension = {
      new Dimension(200, 400)
    }
  }

  val frame = new JFrame() {
    override def processWindowEvent(x: WindowEvent): Unit = {
      if (x.getID == WindowEvent.WINDOW_CLOSING)
        close()
      super.processWindowEvent(x)
    }
  }

  frame.add(canvas)
  frame.setTitle("Test")
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.setVisible(false)
  canvas.createBufferStrategy(3)

  type Buffer = Array[Int]
  override def start(): Unit = {
    javax.swing.SwingUtilities.invokeLater { () =>
      running = true
      frame.setVisible(true)
      thread.start()
    }
  }

  override def paint(f: Buffer => Unit): Future[Unit] = {
    val p = Promise[Unit]
    workQueue.add(f -> p)
    p.future
  }

  override def stop(): Unit = {
    running = false
    thread.join()
  }

  override def close(): Unit = {
    javax.swing.SwingUtilities.invokeLater { () =>
      frame.setVisible(false)
      frame.dispose()
    }
    stop()
  }

  def join(): Unit = thread.join()

}

object TUIRun {
  def main(args: Array[String]): Unit = {
    println("Hello World !!!")

    val window = new SwingWindow()
    val SCREEN_WIDTH = 400
    window.start()
    window.paint { x =>
      for (i <- (0 to 10)) {
        for (j <- (0 to 20)) {
          x((i * SCREEN_WIDTH) + j) = -1
        }
      }
    }

    window.join()
    window.close()
  }
}
