package example

import me.ngrid.duke.console.{TuiSwing => TS}

object TuiSwing {
  def main(args: Array[String]): Unit = {
    val tui = new TS()
    tui.observer = x => tui.appendText(x + "\n")
  }
}
