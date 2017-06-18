package alogic

// This file describes the classes used in the parser output.

///////////////////////////////////////////////////////////////////////////////
// AST super type
///////////////////////////////////////////////////////////////////////////////
sealed trait AlogicAST

///////////////////////////////////////////////////////////////////////////////
// Task (module) nodes, these are the roots of the ASTs
///////////////////////////////////////////////////////////////////////////////
sealed trait AlogicTask extends AlogicAST {
  val name: String
  val decls: List[Declaration]
}

object AlogicTask {
  def unapply(task: AlogicTask) = Some((task.name, task.decls))
}

case class FsmTask(name: String,
                   decls: List[Declaration],
                   fns: List[Function],
                   fencefn: Option[FenceFunction],
                   vfns: List[VerilogFunction]) extends AlogicTask
case class StateTask(name: String,
                     decls: List[Declaration],
                     states: List[StateBlock],
                     fencefn: Option[FenceFunction],
                     vfns: List[VerilogFunction]) extends AlogicTask
case class NetworkTask(name: String, decls: List[Declaration], fns: List[AlogicAST]) extends AlogicTask
case class VerilogTask(name: String, decls: List[Declaration], fns: List[VerilogFunction]) extends AlogicTask

///////////////////////////////////////////////////////////////////////////////
// Function nodes
///////////////////////////////////////////////////////////////////////////////
case class Function(name: String, body: AlogicStmt) extends AlogicAST
case class FenceFunction(body: AlogicStmt) extends AlogicAST
case class VerilogFunction(body: String) extends AlogicAST

///////////////////////////////////////////////////////////////////////////////
// Expression nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait AlogicExpr extends AlogicAST
case class Num(value: String) extends AlogicExpr // Numbers held as textual representation
case class Literal(value: String) extends AlogicExpr // Strings held as textual representation including quotes
case class FunCall(name: DottedName, args: List[AlogicExpr]) extends AlogicExpr
case class Zxt(numbits: AlogicExpr, expr: AlogicExpr) extends AlogicExpr
case class Sxt(numbits: AlogicExpr, expr: AlogicExpr) extends AlogicExpr
case class DollarCall(name: String, args: List[AlogicExpr]) extends AlogicExpr
case class ReadCall(name: DottedName) extends AlogicExpr
case class LockCall(name: DottedName) extends AlogicExpr
case class UnlockCall(name: DottedName) extends AlogicExpr
case class ValidCall(name: DottedName) extends AlogicExpr
case class WriteCall(name: DottedName, args: List[AlogicExpr]) extends AlogicExpr
case class BinaryOp(lhs: AlogicExpr, op: String, rhs: AlogicExpr) extends AlogicExpr
case class UnaryOp(op: String, lhs: AlogicExpr) extends AlogicExpr
case class Bracket(content: AlogicExpr) extends AlogicExpr // TODO: This node is likely redundant
case class TernaryOp(cond: AlogicExpr, lhs: AlogicExpr, rhs: AlogicExpr) extends AlogicExpr
case class BitRep(count: AlogicExpr, value: AlogicExpr) extends AlogicExpr
case class BitCat(parts: List[AlogicExpr]) extends AlogicExpr
case class Slice(ref: AlogicVarRef, lidx: AlogicExpr, op: String, ridx: AlogicExpr) extends AlogicExpr

///////////////////////////////////////////////////////////////////////////////
// Variable reference nodes (also expressions)
///////////////////////////////////////////////////////////////////////////////
sealed trait AlogicVarRef extends AlogicExpr
case class DottedName(names: List[String]) extends AlogicVarRef
case class ArrayLookup(name: DottedName, index: List[AlogicExpr]) extends AlogicVarRef

///////////////////////////////////////////////////////////////////////////////
// Expression nodes
///////////////////////////////////////////////////////////////////////////////
sealed trait AlogicStmt extends AlogicAST

case class Assign(lhs: AlogicAST, rhs: AlogicExpr) extends AlogicStmt
case class Update(lhs: AlogicExpr, op: String, rhs: AlogicExpr) extends AlogicStmt
case class Plusplus(lhs: AlogicExpr) extends AlogicStmt
case class Minusminus(lhs: AlogicExpr) extends AlogicStmt

case class ExprStmt(expr: AlogicExpr) extends AlogicStmt

case class CombinatorialBlock(cmds: List[AlogicStmt]) extends AlogicStmt
case class DeclarationStmt(decl: VarDeclaration) extends AlogicStmt // Used when a declaration is mixed in with the code
case class CombinatorialIf(cond: AlogicAST, body: AlogicStmt, elsebody: Option[AlogicStmt]) extends AlogicStmt
case class AlogicComment(str: String) extends AlogicStmt
case class CombinatorialCaseStmt(value: AlogicAST, cases: List[AlogicAST]) extends AlogicStmt

case class ControlCaseStmt(value: AlogicAST, cases: List[AlogicAST]) extends AlogicStmt
case class ControlIf(cond: AlogicExpr, body: AlogicStmt, elsebody: Option[AlogicStmt]) extends AlogicStmt
case class ControlBlock(cmds: List[AlogicStmt]) extends AlogicStmt
case class ControlWhile(cond: AlogicExpr, body: List[AlogicStmt]) extends AlogicStmt
case class ControlFor(init: AlogicStmt, cond: AlogicExpr, incr: AlogicStmt, body: List[AlogicStmt]) extends AlogicStmt
case class ControlDo(cond: AlogicExpr, body: List[AlogicStmt]) extends AlogicStmt
case object FenceStmt extends AlogicStmt
case object BreakStmt extends AlogicStmt
case object ReturnStmt extends AlogicStmt
case class GotoStmt(target: String) extends AlogicStmt // tODO: take DottedName ?

// Extra types inserted by MakeStates
case class StateBlock(state: Int, contents: List[AlogicStmt]) extends AlogicStmt
case class GotoState(state: Int) extends AlogicStmt

// Case classes do not support inheritance
// Use sealed trait to force compiler to detect all cases are covered
// Use different abstract classes to decompose problem.

//
// // Declaration used for top-level and function declarations
sealed trait Declaration
// TODO: Add separate Decl for Arrays, it just makes a lot of things simpler
case class VarDeclaration(decltype: AlogicType, id: AlogicAST, init: Option[AlogicExpr]) extends Declaration
case class ParamDeclaration(decltype: AlogicType, id: String, init: AlogicExpr) extends Declaration
case class ConstDeclaration(decltype: AlogicType, id: String, init: AlogicExpr) extends Declaration
case class VerilogDeclaration(decltype: AlogicType, id: AlogicAST) extends Declaration
case class OutDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration
case class InDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration

// // AlogicAST used for abstract syntax nodes
case class Connect(start: AlogicAST, end: List[AlogicAST]) extends AlogicAST
case class Instantiate(id: String, module: String, args: List[AlogicAST]) extends AlogicAST

case class CombinatorialCaseLabel(cond: List[AlogicExpr], body: AlogicStmt) extends AlogicAST

// Types removed by MakeStates
case class ControlCaseLabel(cond: List[AlogicExpr], body: AlogicStmt) extends AlogicAST

// AlogicType used to define the allowed types
sealed trait AlogicType
case class IntType(signed: Boolean, size: Int) extends AlogicType
case class IntVType(signed: Boolean, args: List[AlogicExpr]) extends AlogicType // variable number of bits definition
case class Struct(fields: Map[String, AlogicType]) extends AlogicType
case object State extends AlogicType // Type with enough bits to hold state variable

// SyncType for allowed port types
sealed trait SyncType
case object SyncReadyBubble extends SyncType
case object SyncReady extends SyncType
case object SyncAccept extends SyncType
case object Sync extends SyncType
case object WireSyncAccept extends SyncType
case object WireSync extends SyncType
case object Wire extends SyncType
