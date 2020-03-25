package example

object Hello extends Greeting with App {
  println(world)
}

trait Greeting {
  lazy val world: String = "hello"
}