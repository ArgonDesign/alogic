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
case class Function(name: String, body: CtrlStmt) extends Node
case class FenceFunction(body: CombStmt) extends Node
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
// Statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Stmt extends Node

///////////////////////////////////////////////////////////////////////////////
// Combinatorial statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait CombStmt extends Stmt
case class Assign(lhs: Node, rhs: Expr) extends CombStmt
case class Update(lhs: Expr, op: String, rhs: Expr) extends CombStmt
case class Plusplus(lhs: Expr) extends CombStmt
case class Minusminus(lhs: Expr) extends CombStmt
case class ExprStmt(expr: Expr) extends CombStmt
case class DeclarationStmt(decl: VarDeclaration) extends CombStmt
case class AlogicComment(str: String) extends CombStmt
case class CombinatorialBlock(cmds: List[CombStmt]) extends CombStmt
case class CombinatorialIf(cond: Node, body: CombStmt, elsebody: Option[CombStmt]) extends CombStmt
case class CombinatorialCaseStmt(value: Node, cases: List[CombinatorialCaseLabel], default: Option[CombStmt]) extends CombStmt
case class GotoState(tgt: Int) extends CombStmt // inserted by MakeStates
case class CallState(tgt: Int, ret: Int) extends CombStmt // inserted by MakeStates
case object ReturnState extends CombStmt // inserted by MakeStates

///////////////////////////////////////////////////////////////////////////////
// Control statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait CtrlStmt extends Stmt
case class ControlBlock(cmds: List[Stmt]) extends CtrlStmt
case class ControlIf(cond: Expr, body: CtrlStmt, elsebody: Option[CtrlStmt]) extends CtrlStmt
case class ControlCaseStmt(value: Node, cases: List[ControlCaseLabel], default: Option[CtrlStmt]) extends CtrlStmt
case class ControlWhile(cond: Expr, body: List[Stmt]) extends CtrlStmt
case class ControlFor(init: CombStmt, cond: Expr, incr: CombStmt, body: List[Stmt]) extends CtrlStmt
case class ControlDo(cond: Expr, body: List[Stmt]) extends CtrlStmt
case class GotoStmt(target: String) extends CtrlStmt // TODO: take DottedName ?
case class CallStmt(target: String) extends CtrlStmt
case object ReturnStmt extends CtrlStmt
case object FenceStmt extends CtrlStmt
case object BreakStmt extends CtrlStmt

///////////////////////////////////////////////////////////////////////////////
// Case label nodes
///////////////////////////////////////////////////////////////////////////////
case class CombinatorialCaseLabel(cond: List[Expr], body: CombStmt) extends Node
case class ControlCaseLabel(cond: List[Expr], body: CtrlStmt) extends Node

///////////////////////////////////////////////////////////////////////////////
// Node representing FSM states after control statement conversion
///////////////////////////////////////////////////////////////////////////////
case class StateBlock(state: Int, contents: List[CombStmt]) extends Node

// TODO: strengthen types
case class Connect(start: Node, end: List[Node]) extends Node
case class Instantiate(id: String, module: String, args: List[Node]) extends Node
