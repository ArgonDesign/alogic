// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

final class MakeVerilog {

  val id2decl = mutable.Map[String,Declaration]()
  
  def saveDecl(x:AlogicAST): Unit = x match {
    case DeclarationStmt(d) => id2decl(ExtractName(d)) = d
    case _ => Unit
  }

  def apply(tree:StateProgram, fname: String) : Unit = {
    VisitAST( tree, saveDecl)
  }

}


