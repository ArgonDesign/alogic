package alogic

import scala.collection._

import alogic.ast._
import scala.collection.mutable.Stack

// This class keeps track of the connections of a particular module instance

class ModuleInstance(val name: String, val task: Task, val paramAssigns: Map[String, Expr]) {

  // TODO check in parser that no port name repeats

  // Map from portname to Port with internal name
  val iports: Map[String, Port] = {
    task.decls collect {
      case InDeclaration(SyncReadyBubble, kind, name) => name -> PortReady(name, kind)
      case InDeclaration(SyncReady, kind, name)       => name -> PortReady(name, kind)
      case InDeclaration(SyncAccept, kind, name)      => name -> PortAccept(name, kind)
      case InDeclaration(WireSyncAccept, kind, name)  => name -> PortAccept(name, kind)
      case InDeclaration(Sync, kind, name)            => name -> PortValid(name, kind)
      case InDeclaration(WireSync, kind, name)        => name -> PortValid(name, kind)
      case InDeclaration(Wire, kind, name)            => name -> PortNone(name, kind)
    }
  }.toMap
  val oports: Map[String, Port] = {
    task.decls collect {
      case OutDeclaration(SyncReadyBubble, kind, name) => name -> PortReady(name, kind)
      case OutDeclaration(SyncReady, kind, name)       => name -> PortReady(name, kind)
      case OutDeclaration(SyncAccept, kind, name)      => name -> PortAccept(name, kind)
      case OutDeclaration(WireSyncAccept, kind, name)  => name -> PortAccept(name, kind)
      case OutDeclaration(Sync, kind, name)            => name -> PortValid(name, kind)
      case OutDeclaration(WireSync, kind, name)        => name -> PortValid(name, kind)
      case OutDeclaration(Wire, kind, name)            => name -> PortNone(name, kind)
    }
  }.toMap

  val ports = iports.values.toList ::: oports.values.toList

  private[this] val prefix = if (name == "this") "" else name + "__"

  // Map from portname to Port with external wire names
  val iwires: Map[String, Port] = {
    iports map {
      case (key, PortNone(name, kind))   => key -> PortNone(prefix + name, kind)
      case (key, PortValid(name, kind))  => key -> PortValid(prefix + name, kind)
      case (key, PortReady(name, kind))  => key -> PortReady(prefix + name, kind)
      case (key, PortAccept(name, kind)) => key -> PortAccept(prefix + name, kind)
    }
  }.toMap

  val owires: Map[String, Port] = {
    oports map {
      case (key, PortNone(name, kind))   => key -> PortNone(prefix + name, kind)
      case (key, PortValid(name, kind))  => key -> PortValid(prefix + name, kind)
      case (key, PortReady(name, kind))  => key -> PortReady(prefix + name, kind)
      case (key, PortAccept(name, kind)) => key -> PortAccept(prefix + name, kind)
    }
  }.toMap

  val wires = iwires.values.toList ::: owires.values.toList
}
