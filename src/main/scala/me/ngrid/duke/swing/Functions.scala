package me.ngrid.duke.swing

import java.awt.Dimension

import javax.swing
import javax.swing.JFrame

import scala.util.Try

trait Functions {
  def simpleScaledBltWindow(bufferWidth: Int, bufferHeight: Int, drawDelay: Int): (swing.JFrame, Array[Int]) = {
    val canvas = PixelBltCanvas(bufferWidth, bufferHeight)

    val timer = new swing.Timer (drawDelay, { _ =>
      Try(canvas.draw())
    })

    val jframe: JFrame = new swing.JFrame() {
      override val getPreferredSize: Dimension = new Dimension(500, 500)

      override def dispose(): Unit =  {
        if(timer.isRunning) timer.stop()
        super.dispose()
      }

      override def setVisible(b: Boolean): Unit = {
        if(b && !timer.isRunning) {
          timer.start()
        } else if (!b && timer.isRunning) {
          timer.stop()
        }
        super.setVisible(b)
      }
    }

    jframe.add(canvas)

    jframe -> canvas.pixels
  }
}
