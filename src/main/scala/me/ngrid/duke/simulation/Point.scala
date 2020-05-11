package me.ngrid.duke.simulation

import scala.reflect.ClassTag
import scala.collection.immutable

sealed trait Point[T] extends NDimensional[T] with Serializable with  Product {}

object Point {
    final case class _1D[T: ClassTag](override val x: T) extends NDimensional._1D[T] with Point[T]{
        val offsets = Array(x)
        def +(p: _1D[T])(implicit v: Numeric[T]) = 
            _1D(v.plus(x, p.x))
        def -(p: _1D[T])(implicit v: Numeric[T]) = 
            _1D(v.minus(x, p.x))
    }

    type Distance[T] = _1D[T]
    final case class _2D[T: ClassTag](override val x: T,  override val y: T) extends NDimensional._2D[T] {
        val offsets = Array(y, x)
        def +(p: _2D[T])(implicit v: Numeric[T]) = 
            _2D(v.plus(x, p.x), v.plus(y, p.y))
        def -(p: _2D[T])(implicit v: Numeric[T]) = 
            _2D(v.minus(x, p.x), v.minus(y, p.y))
    }
    type Area[T] = _2D[T]
    final case class _3D[T: ClassTag](override val x: T, override val y: T, override val z: T) extends NDimensional._3D[T]  with Point[T] {
        override val offsets = Array(z, y, x)
        def +(p: _3D[T])(implicit v: Numeric[T]) = 
            _3D(v.plus(x, p.x), v.plus(y, p.y), v.plus(z, p.z))
        def -(p: _3D[T])(implicit v: Numeric[T]) = 
            _3D(v.minus(x, p.x), v.minus(y, p.y), v.minus(z, p.z))
    }
    type Volume[T] = _3D[T]
}