package alogic.ast

// This file describes the classes used in the parser output.

///////////////////////////////////////////////////////////////////////////////
// AST node super type
///////////////////////////////////////////////////////////////////////////////
sealed trait Node

///////////////////////////////////////////////////////////////////////////////
// Task (module) nodes, these are the roots of the ASTs
///////////////////////////////////////////////////////////////////////////////
sealed trait Task extends Node {
  val name: String
  val decls: List[Declaration]
}

object Task {
  def unapply(task: Task) = Some((task.name, task.decls))
}

case class FsmTask(name: String,
                   decls: List[Declaration],
                   fns: List[Function],
                   fencefn: Option[FenceFunction],
                   vfns: List[VerilogFunction]) extends Task
case class StateTask(name: String,
                     decls: List[Declaration],
                     states: List[StateBlock],
                     fencefn: Option[FenceFunction],
                     vfns: List[VerilogFunction]) extends Task
case class NetworkTask(name: String, decls: List[Declaration], fns: List[Node]) extends Task
case class VerilogTask(name: String, decls: List[Declaration], fns: List[VerilogFunction]) extends Task

///////////////////////////////////////////////////////////////////////////////
// Function nodes
///////////////////////////////////////////////////////////////////////////////
case class Function(name: String, body: Stmt) extends Node
case class FenceFunction(body: Stmt) extends Node
case class VerilogFunction(body: String) extends Node

///////////////////////////////////////////////////////////////////////////////
// Expression nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Expr extends Node
case class Num(value: String) extends Expr // Numbers held as textual representation
case class Literal(value: String) extends Expr // Strings held as textual representation including quotes
case class CallExpr(name: DottedName, args: List[Expr]) extends Expr
case class Zxt(numbits: Expr, expr: Expr) extends Expr
case class Sxt(numbits: Expr, expr: Expr) extends Expr
case class DollarCall(name: String, args: List[Expr]) extends Expr
case class ReadCall(name: DottedName) extends Expr
case class LockCall(name: DottedName) extends Expr
case class UnlockCall(name: DottedName) extends Expr
case class ValidCall(name: DottedName) extends Expr
case class WriteCall(name: DottedName, args: List[Expr]) extends Expr
case class BinaryOp(lhs: Expr, op: String, rhs: Expr) extends Expr
case class UnaryOp(op: String, lhs: Expr) extends Expr
case class Bracket(content: Expr) extends Expr // TODO: This node is likely redundant
case class TernaryOp(cond: Expr, lhs: Expr, rhs: Expr) extends Expr
case class BitRep(count: Expr, value: Expr) extends Expr
case class BitCat(parts: List[Expr]) extends Expr
case class Slice(ref: VarRef, lidx: Expr, op: String, ridx: Expr) extends Expr

///////////////////////////////////////////////////////////////////////////////
// Variable reference nodes (also expressions)
///////////////////////////////////////////////////////////////////////////////
sealed trait VarRef extends Expr
case class DottedName(names: List[String]) extends VarRef
case class ArrayLookup(name: DottedName, index: List[Expr]) extends VarRef

///////////////////////////////////////////////////////////////////////////////
// Expression nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Stmt extends Node

case class Assign(lhs: Node, rhs: Expr) extends Stmt
case class Update(lhs: Expr, op: String, rhs: Expr) extends Stmt
case class Plusplus(lhs: Expr) extends Stmt
case class Minusminus(lhs: Expr) extends Stmt

case class CallStmt(target: String) extends Stmt

case class ExprStmt(expr: Expr) extends Stmt

case class CombinatorialBlock(cmds: List[Stmt]) extends Stmt
case class DeclarationStmt(decl: VarDeclaration) extends Stmt // Used when a declaration is mixed in with the code
case class CombinatorialIf(cond: Node, body: Stmt, elsebody: Option[Stmt]) extends Stmt
case class AlogicComment(str: String) extends Stmt
case class CombinatorialCaseStmt(value: Node, cases: List[Node]) extends Stmt

case class ControlCaseStmt(value: Node, cases: List[Node]) extends Stmt
case class ControlIf(cond: Expr, body: Stmt, elsebody: Option[Stmt]) extends Stmt
case class ControlBlock(cmds: List[Stmt]) extends Stmt
case class ControlWhile(cond: Expr, body: List[Stmt]) extends Stmt
case class ControlFor(init: Stmt, cond: Expr, incr: Stmt, body: List[Stmt]) extends Stmt
case class ControlDo(cond: Expr, body: List[Stmt]) extends Stmt
case object FenceStmt extends Stmt
case object BreakStmt extends Stmt
case object ReturnStmt extends Stmt
case class GotoStmt(target: String) extends Stmt // tODO: take DottedName ?

// Extra types inserted by MakeStates
case class StateBlock(state: Int, contents: List[Stmt]) extends Stmt
case class GotoState(tgt: Int) extends Stmt
case class CallState(tgt: Int, ret: Int) extends Stmt
case object ReturnState extends Stmt

// // AlogicAST used for abstract syntax nodes
case class Connect(start: Node, end: List[Node]) extends Node
case class Instantiate(id: String, module: String, args: List[Node]) extends Node

case class CombinatorialCaseLabel(cond: List[Expr], body: Stmt) extends Node

// Types removed by MakeStates
case class ControlCaseLabel(cond: List[Expr], body: Stmt) extends Node
