package alogic

import scala.collection._

import alogic.ast._

// This class keeps track of the connections of a particular module instance

class ModuleInstance(val name: String, val task: Task, val params: List[ParamAssign]) {

  // Map from portname to declaration
  val outs = {
    task.decls collect {
      case d @ OutDeclaration(_, _, name) => name -> d // TODO check in parser that no port name repeats
    }
  }.toMap
  val ins = {
    task.decls collect {
      case d @ InDeclaration(_, _, name) => name -> d // TODO check in parser that no port name repeats
    }
  }.toMap

  // Map from portname to wire to be used
  val outwires = mutable.Map[String, String]()
  val inwires = mutable.Map[String, String]()

  def connect(port: String, to: List[(DottedName, ModuleInstance)]): Unit = {
    // TODO check in parser that ports have appropriate lengths

    // Name the wires after the name of the module _ name of port
    // For the toplevel connections we will use assigns to connect the pure names to our local names
    // We could try and reduce the number of signals in certain cases at the cost of making this part a bit less regular.

    val wirename = name + "_" + port
    outwires(port) = wirename
    to foreach {
      case (dest: DottedName, m: ModuleInstance) => {
        val portname = if (dest.names.length > 1) dest.names(1) else port
        m.inwires(port) = wirename // TODO check portname is in port list

        // TODO work out how to connect modules with different declaration types?
        // Need to have matching sync types, but declaration just needs to match total size
        // Could make this illegal, and force modules to do it internally - may make system safer overall
      }
    }
  }

}
