package alogic

// This class keeps track of the connections of a particular module instance

import scala.collection._
import java.io._

class ModuleInstance(val uname: String, val module: String, val args: List[AlogicAST]) {

  // Gather the ports from the AST
  val task: Task = AlogicMain.portMap(module) // TODO check before phase 2 that this module exists

  val outs = mutable.Map[String, OutDeclaration]() // Map from portname to declaration
  val outwires = mutable.Map[String, String]() // From portname to wire to be used
  val ins = mutable.Map[String, InDeclaration]()
  val inwires = mutable.Map[String, String]()

  for (d <- task.decls) d match {
    case d @ OutDeclaration(_, _, name) => outs(name) = d // TODO check in parser that no port name repeats
    case d @ InDeclaration(_, _, name)  => ins(name) = d
    case _                              =>
  }

  def connect(port: DottedName, to: List[(DottedName, ModuleInstance)]): Unit = {
    // TODO check in parser that ports have appropriate lengths

    // Name the wires after the name of the module _ name of port
    // For the toplevel connections we will use assigns to connect the pure names to our local names
    // We could try and reduce the number of signals in certain cases at the cost of making this part a bit less regular.

    val n = port.names(0) // TODO confirm that this matches our name
    val p = port.names(1)
    val wirename = n + "_" + p
    outwires(p) = wirename
    to.foreach(x => {
      val (dest: DottedName, m: ModuleInstance) = x
      val portname = if (dest.names.length > 1)
        dest.names(1)
      else
        p
      m.inwires(p) = wirename // TODO check portname is in port list

      // TODO work out how to connect modules with different declaration types?
      // Need to have matching sync types, but declaration just needs to match total size
      // Could make this illegal, and force modules to do it internally - may make system safer overall
    })
  }

}
