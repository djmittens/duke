package me.ngrid.duke.swing

import java.awt.image.{BufferedImage, DataBufferInt}
import java.awt.{Canvas, Color}

final class PixelBltCanvas(settings: PixelBltCanvas.Settings) extends Canvas {
  import settings._

  val (image, pixels) = {
    val i = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val p = i.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    i -> p
  }

  def draw(): Unit = {
    val bs = {
      val b = getBufferStrategy
      if (b == null) {
        createBufferStrategy(3)
        getBufferStrategy
      } else b
    }
    bs.getDrawGraphics
      .drawImage(image, 0, 0, getWidth, getHeight, null)
    bs.show()
  }


  def clear(color: Color): Unit = {
    val c = color.getRGB
    for(i <- pixels.indices) {
      pixels(i) = c
    }
  }
}

object PixelBltCanvas {
  def apply(width: Int, height: Int): PixelBltCanvas =
    new PixelBltCanvas(Settings(width, height))

  final case class Settings(
      width: Int,
      height: Int
  )
}
