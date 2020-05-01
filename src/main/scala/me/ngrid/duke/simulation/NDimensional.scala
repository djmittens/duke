package me.ngrid.duke.simulation
import Numeric.Implicits._

trait NDimensional[T] {
    def offsets: Seq[T]
    def dimensions: Int
}

object NDimensional {
    trait _1D[T] extends NDimensional[T]  {
        val dimensions: Int = 1
        def x: T = offsets(0)
        def width: T = x
    }

    trait _2D[T] extends _1D[T] {
        override val dimensions: Int = 2
        def y: T = offsets(1)
        def height: T = y

        def area(implicit num: Numeric[T]): T = x * y
    }

    trait _3D[T] extends _2D[T] {
        override val dimensions: Int = 3
        def z: T = offsets(2)
        def depth: T = z

        def volume(implicit num: Numeric[T]): T = x * y * z
    }
}