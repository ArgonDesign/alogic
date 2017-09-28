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

object Node {
  def unapply(node: Node) = Some(node.attr)
}

///////////////////////////////////////////////////////////////////////////////
// Task (module) nodes, these are the roots of the ASTs
///////////////////////////////////////////////////////////////////////////////
sealed trait Task extends Node with TaskOps

object Task {
  def unapply(task: Task) = Some((task.attr, task.name, task.decls))
}

case class FsmTask(attr: Attr,
                   name: String,
                   decls: List[Decl],
                   fns: List[Function],
                   fencefn: Option[FenceFunction],
                   vfns: List[VerilogFunction]) extends Task
case class StateTask(attr: Attr,
                     name: String,
                     decls: List[Decl],
                     states: List[StateBlock],
                     fencefn: Option[FenceFunction],
                     vfns: List[VerilogFunction]) extends Task
case class VerilogTask(attr: Attr,
                       name: String,
                       decls: List[Decl],
                       vfns: List[VerilogFunction]) extends Task
case class NetworkTask(attr: Attr,
                       name: String,
                       decls: List[Decl],
                       instantiate: List[Instantiate],
                       connect: List[Connect],
                       vfns: List[VerilogFunction],
                       fsms: List[FsmTask]) extends Task

///////////////////////////////////////////////////////////////////////////////
// Function nodes
///////////////////////////////////////////////////////////////////////////////
case class Function(attr: Attr, name: String, body: CtrlStmt) extends Node
case class FenceFunction(attr: Attr, body: CombStmt) extends Node
case class VerilogFunction(attr: Attr, body: String) extends Node

///////////////////////////////////////////////////////////////////////////////
// Expression nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Expr extends Node with ExprOps
object Expr extends ExprObjOps
// Numbers have signedness and an optional width. If with is  None,
// the number is considered un-sized
case class Num(attr: Attr, signed: Boolean, width: Option[BigInt], value: BigInt) extends Expr
case class CallExpr(attr: Attr, name: DottedName, args: List[Expr]) extends Expr
case class Zxt(attr: Attr, numbits: Expr, expr: Expr) extends Expr
case class Sxt(attr: Attr, numbits: Expr, expr: Expr) extends Expr
case class DollarCall(attr: Attr, name: String, args: List[Expr]) extends Expr
case class ReadCall(attr: Attr, name: DottedName) extends Expr
case class WaitCall(attr: Attr, name: DottedName) extends Expr
case class PipelineRead(attr: Attr) extends Expr
case class PipelineWrite(attr: Attr) extends Expr
case class ValidCall(attr: Attr, name: DottedName) extends Expr
case class WriteCall(attr: Attr, name: DottedName, args: List[Expr]) extends Expr
case class BinaryOp(attr: Attr, lhs: Expr, op: String, rhs: Expr) extends Expr
case class UnaryOp(attr: Attr, op: String, lhs: Expr) extends Expr
case class Bracket(attr: Attr, content: Expr) extends Expr // TODO: This node is likely redundant
case class TernaryOp(attr: Attr, cond: Expr, lhs: Expr, rhs: Expr) extends Expr
case class BitRep(attr: Attr, count: Expr, value: Expr) extends Expr
case class BitCat(attr: Attr, parts: List[Expr]) extends Expr
case class Slice(attr: Attr, ref: Expr, lidx: Expr, op: String, ridx: Expr) extends Expr
case class DottedName(attr: Attr, names: List[String]) extends Expr
case class ExprArrIndex(attr: Attr, name: DottedName, index: List[Expr]) extends Expr
case class ExprVecIndex(attr: Attr, ref: Expr, index: List[Expr]) extends Expr
case class ErrorExpr(attr: Attr) extends Expr // Placeholder expression

///////////////////////////////////////////////////////////////////////////////
// Left Value expressions
///////////////////////////////////////////////////////////////////////////////
sealed trait LVal extends Node with LValOps
case class LValName(attr: Attr, names: List[String]) extends LVal
case class LValArrayLookup(attr: Attr, name: LValName, index: List[Expr]) extends LVal
case class LValSlice(attr: Attr, ref: LVal, lidx: Expr, op: String, ridx: Expr) extends LVal
case class LValCat(attr: Attr, parts: List[LVal]) extends LVal

///////////////////////////////////////////////////////////////////////////////
// String Literal - including " on both ends
///////////////////////////////////////////////////////////////////////////////
case class Literal(attr: Attr, value: String) extends Expr

///////////////////////////////////////////////////////////////////////////////
// Statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait Stmt extends Node

///////////////////////////////////////////////////////////////////////////////
// Combinatorial statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait CombStmt extends Stmt
case class Assign(attr: Attr, lhs: LVal, rhs: Expr) extends CombStmt
case class Update(attr: Attr, lhs: LVal, op: String, rhs: Expr) extends CombStmt
case class Plusplus(attr: Attr, lhs: LVal) extends CombStmt
case class Minusminus(attr: Attr, lhs: LVal) extends CombStmt
case class ExprStmt(attr: Attr, expr: Expr) extends CombStmt
case class DeclarationStmt(attr: Attr, decl: DeclVar) extends CombStmt
case class AlogicComment(attr: Attr, str: String) extends CombStmt
case class CombinatorialBlock(attr: Attr, cmds: List[CombStmt]) extends CombStmt
case class CombinatorialIf(attr: Attr, cond: Expr, body: CombStmt, elsebody: Option[CombStmt]) extends CombStmt
case class CombinatorialCaseStmt(attr: Attr, value: Expr, cases: List[CombinatorialCaseLabel], default: Option[CombStmt]) extends CombStmt
case class GotoState(attr: Attr, tgt: Int) extends CombStmt // inserted by MakeStates
case class CallState(attr: Attr, tgt: Int, ret: Int) extends CombStmt // inserted by MakeStates
case class ReturnState(attr: Attr) extends CombStmt // inserted by MakeStates

///////////////////////////////////////////////////////////////////////////////
// Control statement nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait CtrlStmt extends Stmt
case class ControlBlock(attr: Attr, cmds: List[Stmt]) extends CtrlStmt
case class ControlIf(attr: Attr, cond: Expr, body: CtrlStmt, elsebody: Option[CtrlStmt]) extends CtrlStmt
case class ControlCaseStmt(attr: Attr, value: Expr, cases: List[ControlCaseLabel], default: Option[CtrlStmt]) extends CtrlStmt
case class ControlLoop(attr: Attr, body: ControlBlock) extends CtrlStmt
case class ControlWhile(attr: Attr, cond: Expr, body: List[Stmt]) extends CtrlStmt
case class ControlFor(attr: Attr, init: CombStmt, cond: Expr, incr: CombStmt, body: List[Stmt]) extends CtrlStmt
case class ControlDo(attr: Attr, cond: Expr, body: List[Stmt]) extends CtrlStmt
case class GotoStmt(attr: Attr, target: String) extends CtrlStmt // TODO: take DottedName ?
case class CallStmt(attr: Attr, target: String) extends CtrlStmt
case class ReturnStmt(attr: Attr) extends CtrlStmt
case class FenceStmt(attr: Attr) extends CtrlStmt
case class BreakStmt(attr: Attr) extends CtrlStmt

///////////////////////////////////////////////////////////////////////////////
// Used where a statement is required but an error has previously occurred
///////////////////////////////////////////////////////////////////////////////
case class ErrorStmt(attr: Attr) extends CombStmt with CtrlStmt // Placeholder statement

///////////////////////////////////////////////////////////////////////////////
// Case label nodes
///////////////////////////////////////////////////////////////////////////////
case class CombinatorialCaseLabel(attr: Attr, cond: List[Expr], body: CombStmt) extends Node
case class ControlCaseLabel(attr: Attr, cond: List[Expr], body: CtrlStmt) extends Node

///////////////////////////////////////////////////////////////////////////////
// Node representing FSM states after control statement conversion
///////////////////////////////////////////////////////////////////////////////
case class StateBlock(attr: Attr, state: Int, contents: List[CombStmt]) extends Node

///////////////////////////////////////////////////////////////////////////////
// Network contents
///////////////////////////////////////////////////////////////////////////////
case class Connect(attr: Attr, lhs: DottedName, rhs: List[DottedName]) extends Node
case class Instantiate(attr: Attr, id: String, module: String, args: Map[String, Expr]) extends Node
