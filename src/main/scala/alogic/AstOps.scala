// This file contains some useful functions for manipulating the abstract syntax tree

package alogic

object AstOps {
  def ExtractName(tree:AlogicAST) : String = tree match {
    case DottedName(ns) => ns.head
    case ArrayLookup(a,_) => ExtractName(a)
    case BinaryArrayLookup(a,_,_,_) => ExtractName(a)
    case _ => "Unknown"
  }
  
  def ExtractName(tree:Declaration) : String = tree match {
    case VarDeclaration(_,id,_) => ExtractName(id)
    case ParamDeclaration(_,id,_) => id
    case VerilogDeclaration(_,id) => ExtractName(id)
    case OutDeclaration(_,_,name) => name
    case InDeclaration(_,_,name) => name
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
  
  // Recurse through the tree and apply function to all nodes in pre-order
  def VisitAST(tree: AlogicAST, callback: AlogicAST => Unit): Unit = {
    callback(tree)
    tree match {
      case Function(name, body) => VisitAST(body,callback)
      case FenceFunction(body) => VisitAST(body,callback)
      case Task(tasktype, name, decls, fns) => for (f <- fns) VisitAST(f,callback)
      case ArrayLookup(name, index) => {VisitAST(name,callback); VisitAST(index,callback)}
      case BinaryArrayLookup(name, lhs, op, rhs) => {VisitAST(name,callback); VisitAST(lhs,callback); VisitAST(rhs,callback)}
      case FunCall(name, args) => {VisitAST(name,callback); for (f <- args) VisitAST(f,callback)}
      case Zxt(numbits, expr) => {VisitAST(numbits,callback); VisitAST(expr,callback)}
      case Sxt(numbits, expr) => {VisitAST(numbits,callback); VisitAST(expr,callback)}
      case DollarCall(name, args) => for (f <- args) VisitAST(f,callback)
      case ReadCall(name, args) => for (f <- args) VisitAST(f,callback)
      case WriteCall(name, args) => for (f <- args) VisitAST(f,callback)
      case Assign(lhs, op, rhs) => {VisitAST(lhs,callback); VisitAST(rhs,callback)}
      case Plusplus(lhs) => VisitAST(lhs,callback)
      case Minusminus(lhs) => VisitAST(lhs,callback)
      case BinaryOp(lhs, op, rhs) => {VisitAST(lhs,callback); VisitAST(rhs,callback)}
      case UnaryOp(op, lhs) => VisitAST(lhs,callback)
      case Bracket(content) => VisitAST(content,callback)
      case TernaryOp(cond, lhs, rhs) => {VisitAST(cond,callback); VisitAST(lhs,callback); VisitAST(rhs,callback)}
      case CombinatorialBlock(cmds) => for (f <- cmds) VisitAST(f,callback)
      case DeclarationStmt(decl:VarDeclaration) => () // TODO should we recurse here?
      case CombinatorialIf(cond, body, Some(e)) => {VisitAST(cond,callback); VisitAST(body,callback); VisitAST(e,callback)}
      case CombinatorialIf(cond, body, None) => {VisitAST(cond,callback); VisitAST(body,callback)}
      case BitRep(count,value)  => {VisitAST(count,callback); VisitAST(value,callback)}
      case BitCat(parts) => for (f <- parts) VisitAST(f,callback)
      case AlogicComment(str) => 
      case CombinatorialCaseStmt(value,cases) => {VisitAST(value,callback); for {f <- cases} VisitCase(f,callback)}
      case Define() =>
      case Typedef() =>
      case Program(cmds) => for {f <- cmds} VisitAST(f,callback)
      case ControlCaseStmt(value,cases) => {VisitAST(value,callback); for {f <- cases} VisitCase(f,callback)}
      case ControlIf(cond, body, Some(e)) => {VisitAST(cond,callback); VisitAST(body,callback); VisitAST(e,callback)}
      case ControlIf(cond, body, None) => {VisitAST(cond,callback); VisitAST(body,callback)}
      case ControlBlock(cmds) => for (f <- cmds) VisitAST(f,callback)
      case WhileLoop(cond, body) => {VisitAST(cond,callback); VisitAST(body,callback)}
      case ControlFor(init, cond, incr, body) => {VisitAST(init,callback); 
                                                   VisitAST(cond,callback); 
                                                   VisitAST(incr,callback); 
                                                   for {f <- body} VisitAST(f,callback)}
      case ControlDo(cond, body) => {VisitAST(cond,callback); for {f <- body} VisitAST(f,callback)}
      case FenceStmt() =>
      case BreakStmt() =>
      case ReturnStmt() =>
      case GotoStmt(target:String) =>
      case StateProgram(cmds, numStates) => for {f <- cmds} VisitAST(f,callback)
      case StateStmt(state) =>
      case GotoState(state) =>
      case DottedName(names) =>
      case Literal(_) =>
      case Num(_) =>
      case VerilogFunction(_) =>
    }
  }  
  
  def VisitCase(tree: CaseLabel, callback: AlogicAST => Unit): Unit = tree match {
    case ControlCaseLabel(cond,body) => {
      for {f <- cond} VisitAST(f,callback)
      VisitAST(body,callback)
    }
    case CombinatorialCaseLabel(cond,body) => {
      for {f <- cond} VisitAST(f,callback)
      VisitAST(body,callback)
    }
  }
  
}
