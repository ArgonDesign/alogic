// This file contains some useful functions for manipulating the abstract syntax tree

package alogic

object AstOps {
  def ExtractName(tree:AlogicAST) : String = tree match {
    case DottedName(ns) => ns.head
    case ArrayLookup(a,_) => ExtractName(a)
    case BinaryArrayLookup(a,_,_,_) => ExtractName(a)
    case _ => "Unknown"
  }
  
  def is_control_stmt(cmd: AlogicAST) : Boolean = cmd match {
    case FenceStmt() => true
    case BreakStmt() => true
    case ReturnStmt() => true
    case GotoStmt(target) => true
    case ControlBlock(s) => true
    case ControlIf(cond,body,elsebody) => true
    case WhileLoop(cond,body) => true
    case ControlFor(_,_,_,_) => true
    case ControlDo(_,_) => true
    case ControlCaseStmt(_,_) => true
    case FunCall(_,_) => true
    case _ => false
  }
}
