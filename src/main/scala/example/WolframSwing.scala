package example

import java.awt.image.{BufferedImage, DataBufferInt}
import java.awt.{Canvas, Color, Dimension}
import java.util.concurrent.atomic.AtomicInteger

import javax.swing.{JFrame, Timer, WindowConstants}
import me.ngrid.duke.simulation.Simulation
import me.ngrid.duke.simulation.ca.{CellularAutomaton, WolframCode}

import scala.util.Random

class WolframSwing(dim: Int, code: Int) {
  val jframe: JFrame = new JFrame() {
    override def getPreferredSize: Dimension = new Dimension(200, 200)
  }

  val (img, pixels) = {

    val i = new BufferedImage(dim, dim, BufferedImage.TYPE_INT_RGB)
    val p = i.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    i -> p
  }

  object sim {
    val init: Array[Boolean] = Array.ofDim[Boolean](dim)
    // seed this mofo
    init(init.length / 2) = true
    val game = new Simulation(CellularAutomaton.x1D(init))
    var wc = new WolframCode(code)

    def tick(): Unit = game.advance { ca => ca.update(wc) }

    def readState(f: Array[Boolean] => Unit): Unit = {
      game.query(f0 => f(f0.cells))
    }
  }

  jframe.setVisible(true)

  val canvas: Canvas = {
    val c = new Canvas()
    jframe.add(c)
    c
  }

  val renderLoop = new Timer(10, { _ =>
    val bs = {
      val b = canvas.getBufferStrategy
      if (b == null) {
        canvas.createBufferStrategy(3)
        canvas.getBufferStrategy
      } else b
    }
    bs.getDrawGraphics.drawImage(img, 0, 0, canvas.getWidth, canvas.getHeight,  null)
    bs.show()
  })

  val simLoop = new Timer(60, { _ =>
    sim.readState { s =>
      // shift every pixel up
      for(i <- 0 until (dim * (dim - 1))) {
        pixels(i) = pixels(i + dim)
      }
      // fetch fresh state
      for (i <- (0 until dim).reverse) {
        val x = pixels.length - 1 - i
        pixels(x) = if(s(i)) Color.GREEN.getRGB else 0
      }
    }
    sim.tick()
  })

  val codeSwitchLoop = new Timer(1000, { _ =>
//    val newcode = if(sim.wc.code < 256) sim.wc.code + 1 else 0
    val newcode = Random.nextInt(256)
    sim.wc = new WolframCode(newcode)
    jframe.setTitle(s"Wolfram Code ##${newcode}##")
  })

  jframe.setTitle(s"Wolfram Code ##${sim.wc.code}##")
  jframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  renderLoop.start()
  simLoop.start()
//  codeSwitchLoop.start()
}

object WolframSwing {
  def main(args: Array[String]): Unit = {
    new WolframSwing(128, 161)
  }
}
