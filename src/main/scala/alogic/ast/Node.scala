////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

// This file describes the classes used in the parser output.

///////////////////////////////////////////////////////////////////////////////
// AST node super type
///////////////////////////////////////////////////////////////////////////////
sealed trait Node extends NodeOps

///////////////////////////////////////////////////////////////////////////////
// Task (module) nodes, these are the roots of the ASTs
///////////////////////////////////////////////////////////////////////////////
sealed trait Task extends Node with TaskOps

object Task {
  def unapply(task: Task) = Some((task.name, task.decls))
}

case class FsmTask(name: String,
                   decls: List[Decl],
                   fns: List[Function],
                   fencefn: Option[FenceFunction],
                   vfns: List[VerilogFunction],
                   makeNew: Boolean) extends Task
case class StateTask(name: String,
                     decls: List[Decl],
                     states: List[StateBlock],
                     fencefn: Option[FenceFunction],
                     vfns: List[VerilogFunction]) extends Task
case class VerilogTask(name: String,
                       decls: List[Decl],
                       vfns: List[VerilogFunction]) extends Task
case class NetworkTask(name: String,
                       decls: List[Decl],
                       instantiate: List[Instantiate],
                       connect: List[Connect],
                       vfns: List[VerilogFunction],
                       fsms: List[FsmTask]) extends Task

///////////////////////////////////////////////////////////////////////////////
// Function nodes
///////////////////////////////////////////////////////////////////////////////
case class Function(name: String, body: CtrlStmt) extends Node
case class FenceFunction(body: CombStmt) extends Node
case class VerilogFunction(body: String) extends Node

///////////////////////////////////////////////////////////////////////////////
// Expression nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Expr extends Node with ExprOps
object Expr extends ExprObjOps
// Numbers have signedness and an optional width. If with is  None,
// the number is considered un-sized
case class Num(signed: Boolean, width: Option[BigInt], value: BigInt) extends Expr
case class CallExpr(name: DottedName, args: List[Expr]) extends Expr
case class Zxt(numbits: Expr, expr: Expr) extends Expr
case class Sxt(numbits: Expr, expr: Expr) extends Expr
case class DollarCall(name: String, args: List[Expr]) extends Expr
case class ReadCall(name: DottedName) extends Expr
case class WaitCall(name: DottedName) extends Expr
case object PipelineRead extends Expr
case object PipelineWrite extends Expr
case class ValidCall(name: DottedName) extends Expr
case class WriteCall(name: DottedName, args: List[Expr]) extends Expr
case class BinaryOp(lhs: Expr, op: String, rhs: Expr) extends Expr
case class UnaryOp(op: String, lhs: Expr) extends Expr
case class Bracket(content: Expr) extends Expr // TODO: This node is likely redundant
case class TernaryOp(cond: Expr, lhs: Expr, rhs: Expr) extends Expr
case class BitRep(count: Expr, value: Expr) extends Expr
case class BitCat(parts: List[Expr]) extends Expr
case class Slice(ref: Expr, lidx: Expr, op: String, ridx: Expr) extends Expr
case class DottedName(names: List[String]) extends Expr
case class ExprArrIndex(name: DottedName, index: List[Expr]) extends Expr
case class ExprVecIndex(ref: Expr, index: List[Expr]) extends Expr
case object ErrorExpr extends Expr // Placeholder expression

///////////////////////////////////////////////////////////////////////////////
// Left Value expressions
///////////////////////////////////////////////////////////////////////////////
sealed trait LVal extends Node with LValOps
case class LValName(names: List[String]) extends LVal
case class LValArrayLookup(name: LValName, index: List[Expr]) extends LVal
case class LValSlice(ref: LVal, lidx: Expr, op: String, ridx: Expr) extends LVal
case class LValCat(parts: List[LVal]) extends LVal

///////////////////////////////////////////////////////////////////////////////
// String Literal - including " on both ends
///////////////////////////////////////////////////////////////////////////////
case class Literal(value: String) extends Expr

///////////////////////////////////////////////////////////////////////////////
// Statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Stmt extends Node

///////////////////////////////////////////////////////////////////////////////
// Combinatorial statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait CombStmt extends Stmt
case class Assign(lhs: LVal, rhs: Expr) extends CombStmt
case class Update(lhs: LVal, op: String, rhs: Expr) extends CombStmt
case class Plusplus(lhs: LVal) extends CombStmt
case class Minusminus(lhs: LVal) extends CombStmt
case class ExprStmt(expr: Expr) extends CombStmt
case class DeclarationStmt(decl: DeclVar) extends CombStmt
case class AlogicComment(str: String) extends CombStmt
case class CombinatorialBlock(cmds: List[CombStmt]) extends CombStmt
case class CombinatorialIf(cond: Expr, body: CombStmt, elsebody: Option[CombStmt]) extends CombStmt
case class CombinatorialCaseStmt(value: Expr, cases: List[CombinatorialCaseLabel], default: Option[CombStmt]) extends CombStmt
case class GotoState(tgt: Int) extends CombStmt // inserted by MakeStates
case class CallState(tgt: Int, ret: Int) extends CombStmt // inserted by MakeStates
case object ReturnState extends CombStmt // inserted by MakeStates

///////////////////////////////////////////////////////////////////////////////
// Control statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait CtrlStmt extends Stmt
case class ControlBlock(cmds: List[Stmt]) extends CtrlStmt
case class ControlIf(cond: Expr, body: CtrlStmt, elsebody: Option[CtrlStmt]) extends CtrlStmt
case class ControlCaseStmt(value: Expr, cases: List[ControlCaseLabel], default: Option[CtrlStmt]) extends CtrlStmt
case class ControlLoop(body: ControlBlock) extends CtrlStmt
case class ControlWhile(cond: Expr, body: List[Stmt]) extends CtrlStmt
case class ControlFor(init: CombStmt, cond: Expr, incr: CombStmt, body: List[Stmt]) extends CtrlStmt
case class ControlDo(cond: Expr, body: List[Stmt]) extends CtrlStmt
case class GotoStmt(target: String) extends CtrlStmt // TODO: take DottedName ?
case class CallStmt(target: String) extends CtrlStmt
case object ReturnStmt extends CtrlStmt
case object FenceStmt extends CtrlStmt
case object BreakStmt extends CtrlStmt

///////////////////////////////////////////////////////////////////////////////
// Used where a statement is required but an error has previously occurred
///////////////////////////////////////////////////////////////////////////////
case object ErrorStmt extends CombStmt with CtrlStmt // Placeholder statement

///////////////////////////////////////////////////////////////////////////////
// Case label nodes
///////////////////////////////////////////////////////////////////////////////
case class CombinatorialCaseLabel(cond: List[Expr], body: CombStmt) extends Node
case class ControlCaseLabel(cond: List[Expr], body: CtrlStmt) extends Node

///////////////////////////////////////////////////////////////////////////////
// Node representing FSM states after control statement conversion
///////////////////////////////////////////////////////////////////////////////
case class StateBlock(state: Int, contents: List[CombStmt]) extends Node

///////////////////////////////////////////////////////////////////////////////
// Network contents
///////////////////////////////////////////////////////////////////////////////
case class Connect(lhs: DottedName, rhs: List[DottedName]) extends Node
case class Instantiate(id: String, module: String, args: Map[String, Expr]) extends Node
