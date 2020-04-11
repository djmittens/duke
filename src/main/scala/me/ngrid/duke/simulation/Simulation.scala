package me.ngrid.duke.simulation

import java.util.concurrent.ConcurrentLinkedQueue

import me.ngrid.duke.protocol.Event


// I am going to put a pin on this thing for now, as i realized that i have never made an actual cellular automata.
class Simulation(systems: Seq[_]) {
  import Simulation._

  var running = true

  // InStream
  val workBuffer = new ConcurrentLinkedQueue[(CommandRef, Simulation.Command)]()

  // OutStream
  val eventBuffer = new ConcurrentLinkedQueue[(CommandRef, Simulation.Event)]()
  val errorBuffer = new ConcurrentLinkedQueue[(CommandRef, Simulation.Event)]()

  val state: State = Map.empty

//  val loop: Thread = {() =>
//    while (running) {
//      val (cref, cmd) = workBuffer.poll()
//      state.foreach {case (eid, comp) =>
////        systems.foreach {_ (eid, cmd, comp)}
//        ???
//      }
//    }
//  }

  /*
  Each system gets its own command buffer.

  System(Command, Components) -> Events()s
  Systems(In, Out):
  Chat
  Simulation
  Combat
  Movement
   */


  def run (): Unit = {

  }
}

object Simulation {
  // Why am i doing ecs? is this healthy?
  type EntityId = Int
  type Component = Int
  type CommandRef = Short
  type UnknownState  = Object // Dont give a fuck about state type, its auto magic.
  type StdOut =  Seq[Event]
  type ErrOut = Seq[Event]
  type ComponentStates = Map[Component, UnknownState]
  type State = Map[EntityId, ComponentStates]

//  type System[Command] = {
//    // Is this a good idea?
//    def apply(e: EntityId, v: Command, c: ComponentStates): (ErrOut, StdOut)
//  }

  sealed trait Command extends Serializable with Product
  object Command {
    final case class NewEntity(seq: Int, id: EntityId) extends Command
  }

  sealed trait Event extends Serializable with Product
  object Event {
    final case class SystemMessage() extends Event
  }
}

object SimulationRun {
  def main(args: Array[String]): Unit = {
  }
}