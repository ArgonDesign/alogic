package alogic

// This class keeps track of the connections of a particular module instance

class ModuleInstance(val module: String, val args: List[AlogicAST]) {

  def connect(port: DottedName, to: List[(DottedName, ModuleInstance)]): Unit = {
    // TODO
  }
}
