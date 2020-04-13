package me.ngrid.duke.util

import scala.reflect.ClassTag

final class NArray[N, T] private(val underlying: Array[T])

case object NArray {
  def apply[N <: Integer, T : ClassTag]()(implicit v: ValueOf[N]): NArray[N, T] = {
    new NArray(Array.ofDim[T](v.value))
  }

  def apply[N <: Int, T : ClassTag](a: Array[T])(implicit v: ValueOf[N]): NArray[N, T] = {
    if(a.length == v.value) {new NArray(a)}
    else throw new IllegalArgumentException(s"The array being constructed(${a.length}) is not of the correct size.(${v.value})")
  }
}