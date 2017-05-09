// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

final class MakeVerilog {

  val id2decl = mutable.Map[String,Declaration]()

  def apply(tree:StateProgram, fname: String) : Unit = {
    tree.cmds.map(extractVariables)
  }
  
  // extractVariables is used to populate our maps from id to Declaration
  def extractVariables(tree: AlogicAST) : Unit = tree match {
    case Task(t,n,decls,fns) => fns.map(extractFunVariables)
    case DeclarationStmt(d) => id2decl(ExtractName(d)) = d
    case _ =>
  }
  
  def extractFunVariables(tree: TaskContent) : Unit = tree match {
    case Function(name,body) => extractVariables(body)
    case _ => 
  }

}


