package me.ngrid.duke.swing

import java.awt.Dimension
import java.awt.event.{KeyListener, MouseEvent, MouseListener}

import example.GameOfLifeSwing
import javax.swing
import javax.swing.{JFrame, WindowConstants}

import scala.util.Try
import java.awt.Point

trait SwingFunctions {
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

      override def addMouseListener(l: MouseListener): Unit = {
        super.addMouseListener(l)
        canvas.addMouseListener(l)
      }

      override def addKeyListener(l: KeyListener): Unit = {
        super.addKeyListener(l)
        canvas.addKeyListener(l)
      }

      override def getMousePosition(): Point = {
        canvas.getMousePosition()
      }
    }

    jframe.add(canvas)

    jframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    jframe -> canvas.pixels
  }

}
