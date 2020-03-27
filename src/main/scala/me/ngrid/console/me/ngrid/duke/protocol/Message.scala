package me.ngrid.console.me.ngrid.duke.protocol

import java.util.UUID

sealed trait Message extends Serializable with Product

object Message {
  sealed trait Client extends Message
  final case class SYN(seq: Int) extends Client
  final case class HELLO(
      protocolVersion: Int,
      clientVersion: Int,
      clientTime: Long,
      clientName: String
  ) extends Client
  final case class COMMAND[V](value: V) extends Client
  final case class QUERY[V](seq: Int, value: V) extends Client
  sealed trait Server extends Message
  final case class WELCOME(
      serverVersion: Int,
      serverTime: Long) extends Server
  final case class EVENT[V](value: V) extends Server
  final case class RESULT[V](seq: Int, value: V) extends Server
  final case class BYE(refId: UUID) extends Server
  final case class ACK(seq: Int) extends Server
}
