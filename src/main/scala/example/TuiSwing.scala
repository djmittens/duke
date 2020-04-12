package example

import me.ngrid.duke.console.TuiSwing

object TuiSwing {
  def main(args: Array[String]): Unit = {
    val tui = new TuiSwing()
    tui.observer = x => tui.appendText(x + "\n")
  }
}
