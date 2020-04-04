package me.ngrid.duke.protocol

sealed trait Event extends Serializable with Product {}
object Event {
  final case class Say(entityId: String, message: String) extends Event
}
