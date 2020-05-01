package example

class GoLEditor {
    final case class ViewPort(u: Double, v: Double, x: Int, y: Int) {
        def move(u: Double, v: Double) = copy(u = u, v = v)
    }

    final case class Buffer2D[T](x: Int, y: Int, underlying: Array[T])

    def render(viewport: ViewPort, buffer: Buffer2D[Int]) = {
        /*
        simulation.query(s => s.get)
        */
    }
}

object GoLEditor {
    def main(args: Array[String]): Unit = {
        println("Hello Wolrd")
    }
}