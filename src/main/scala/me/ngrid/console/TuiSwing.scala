package me.ngrid.console
import java.awt._
import java.util.concurrent.ConcurrentLinkedQueue

import javax.swing._

class TuiSwing {
  val jframe = new JFrame {
    override def getPreferredSize(): Dimension = new Dimension(400, 400)
  }
  val jfield = new JTextField()
  jfield.setBounds(50, 50, 150, 20)

  val workQueue = new ConcurrentLinkedQueue[TuiSwing.Event]()

  val toolbar = new JPanel()

  val jtpane = new JTextPane() {
      // Turn on antialiasing for the font
      override def paintComponent(x: Graphics): Unit = {
          val g = x.asInstanceOf[Graphics2D]
          // antialias geometric shapes
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          // antialias for text
          g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
          super.paintComponent(g)
      }
  }

  var observer: TuiSwing.Observer = null

  val updateLoop = new Timer(17, { _ =>
    var batch = 100
    while (!workQueue.isEmpty() && batch > 0) {
      workQueue.poll() match {
        case TuiSwing.NewText(txt, attr) =>
          val doc = jtpane.getDocument()
          // this little bit is able to delete the top of the screen
          //   doc.remove(0, doc.getLength())
          doc.insertString(doc.getLength, txt, attr)
          // scrolls down
          val r = jtpane.modelToView2D(doc.getLength()).asInstanceOf[Rectangle]
          if (r != null) jtpane.scrollRectToVisible(r)
      }
    }
  })

  // JFrame layout
  {
    val mainBox = Box.createHorizontalBox()

    val console = Box.createVerticalBox()
    jtpane.setMinimumSize(new Dimension(0, 0))
    jtpane.setMaximumSize(new Dimension(Short.MaxValue, Short.MaxValue))
    jtpane.setEditable(false)
    jtpane.setAutoscrolls(true)
    console.add(new JScrollPane(jtpane))

    jfield.setMinimumSize(new Dimension(10, 20))
    jfield.setMaximumSize(new Dimension(Short.MaxValue, 20))
    console.add(jfield)

    mainBox.add(console)

    toolbar.setMaximumSize(new Dimension(250, Short.MaxValue))
    toolbar.setMinimumSize(new Dimension(250, 120))
    toolbar.setPreferredSize(new Dimension(250, 120))

    mainBox.add(toolbar)

    jframe.add(mainBox)
  }

  jfield.addActionListener { e =>
    if (observer != null) {
      observer.onCommand(jfield.getText())
    }

    jfield.setText("")
  }

  jframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  jframe.setVisible(true)
  updateLoop.start()

  val simpleAttributes = {
    val attr = new text.SimpleAttributeSet()
    text.StyleConstants.setFontFamily(attr, "Courier New")
    text.StyleConstants.setBackground(attr, Color.white)
    text.StyleConstants.setFontSize(attr, 14)
    attr
  }

  val systemMessage = simpleAttributes.clone()
  def appendText(txt: String): Unit =
    workQueue.offer(TuiSwing.NewText(txt, simpleAttributes))
}

object TuiSwing {
  sealed trait Event extends Serializable with Product

  final case class NewText(txt: String, attributes: text.SimpleAttributeSet)
      extends Event

  trait Observer {
    def onCommand(txt: String): Unit
  }
}

object TuiSwingRun {
  def main(args: Array[String]): Unit = {
    val tui = new TuiSwing()
    tui.observer = x => tui.appendText(x + "\n")
  }
}
