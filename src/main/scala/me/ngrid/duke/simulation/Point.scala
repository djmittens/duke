package me.ngrid.duke.simulation

import scala.reflect.ClassTag
import scala.collection.immutable

sealed trait Point[T] extends NDimensional[T] with Serializable with  Product {}

object Point {
    final case class _1D[T: ClassTag](override val x: T) extends NDimensional._1D[T] with Point[T]{
        val offsets = Array(x)
    }

    type Distance[T] = _1D[T]
    final case class _2D[T: ClassTag](override val x: T,  override val y: T) extends NDimensional._2D[T] {
        val offsets = Array(y, x)
    }
    type Area[T] = _2D[T]
    final case class _3D[T: ClassTag](override val x: T, override val y: T, override val z: T) extends NDimensional._3D[T]  with Point[T] {
        override val offsets = Array(z, y, x)
    }
    type Volume[T] = _3D[T]
}