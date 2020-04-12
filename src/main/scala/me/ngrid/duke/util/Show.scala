package me.ngrid.duke.util

trait Show[T] {
  def apply(v: T): String
}

object Show {
  def apply[T : Show]: Show[T] = implicitly[Show[T]]
}