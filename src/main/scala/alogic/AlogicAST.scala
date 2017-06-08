package alogic

// This file describes the classes used in the parser output.

// Case classes do not support inheritance
// Use sealed trait to force compiler to detect all cases are covered
// Use different abstract classes to decompose problem.

// // TaskType defines different types of modules
sealed trait TaskType
case object Fsm extends TaskType
case object Pipeline extends TaskType
case object Network extends TaskType
case object Verilog extends TaskType
//
// // Declaration used for top-level and function declarations
sealed trait Declaration
case class VarDeclaration(decltype: AlogicType, id: AlogicAST, init: Option[AlogicExpr]) extends Declaration
case class ParamDeclaration(decltype: AlogicType, id: String, init: AlogicExpr) extends Declaration
case class ConstDeclaration(decltype: AlogicType, id: String, init: AlogicExpr) extends Declaration
case class VerilogDeclaration(decltype: AlogicType, id: AlogicAST) extends Declaration
case class OutDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration
case class InDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration

// // AlogicAST used for abstract syntax nodes
sealed trait AlogicAST
case class Connect(start: AlogicAST, end: List[AlogicAST]) extends AlogicAST
case class Instantiate(id: String, module: String, args: List[AlogicAST]) extends AlogicAST
case class Function(name: String, body: AlogicAST) extends AlogicAST
case class FenceFunction(body: AlogicAST) extends AlogicAST
case class VerilogFunction(body: String) extends AlogicAST

case class Task(tasktype: TaskType, name: String, decls: List[Declaration], fns: List[AlogicAST]) extends AlogicAST

case class Assign(lhs: AlogicAST, rhs: AlogicExpr) extends AlogicAST

case class CombinatorialBlock(cmds: List[AlogicAST]) extends AlogicAST
case class DeclarationStmt(decl: VarDeclaration) extends AlogicAST // Used when a declaration is mixed in with the code
case class CombinatorialIf(cond: AlogicAST, body: AlogicAST, elsebody: Option[AlogicAST]) extends AlogicAST
case class AlogicComment(str: String) extends AlogicAST
case class CombinatorialCaseStmt(value: AlogicAST, cases: List[AlogicAST]) extends AlogicAST
case class CombinatorialCaseLabel(cond: List[AlogicExpr], body: AlogicAST) extends AlogicAST

// AST expression nodes
sealed trait AlogicExpr extends AlogicAST
case class Num(value: String) extends AlogicExpr // Numbers held as textual representation
case class Literal(value: String) extends AlogicExpr // Strings held as textual representation including quotes
case class DottedName(names: List[String]) extends AlogicExpr
case class ArrayLookup(name: AlogicAST, index: AlogicExpr) extends AlogicExpr
case class BinaryArrayLookup(name: AlogicAST, lhs: AlogicExpr, op: String, rhs: AlogicExpr) extends AlogicExpr
case class FunCall(name: AlogicAST, args: List[AlogicExpr]) extends AlogicExpr
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

// Types removed by AstBuilder
case object Typedef extends AlogicAST

// Types removed by Desugar
case class Update(lhs: AlogicExpr, op: String, rhs: AlogicExpr) extends AlogicAST
case class Plusplus(lhs: AlogicExpr) extends AlogicAST
case class Minusminus(lhs: AlogicExpr) extends AlogicAST

// Types removed by MakeStates
case class Program(cmds: List[AlogicAST]) extends AlogicAST
case class ControlCaseStmt(value: AlogicAST, cases: List[AlogicAST]) extends AlogicAST
case class ControlIf(cond: AlogicExpr, body: AlogicAST, elsebody: Option[AlogicAST]) extends AlogicAST
case class ControlBlock(cmds: List[AlogicAST]) extends AlogicAST
case class ControlWhile(cond: AlogicExpr, body: List[AlogicAST]) extends AlogicAST
case class ControlFor(init: AlogicAST, cond: AlogicExpr, incr: AlogicAST, body: List[AlogicAST]) extends AlogicAST
case class ControlDo(cond: AlogicExpr, body: List[AlogicAST]) extends AlogicAST
case object FenceStmt extends AlogicAST
case object BreakStmt extends AlogicAST
case object ReturnStmt extends AlogicAST
case class GotoStmt(target: String) extends AlogicAST
case class ControlCaseLabel(cond: List[AlogicExpr], body: AlogicAST) extends AlogicAST

// Extra types inserted by MakeStates
case class StateProgram(cmds: List[AlogicAST], numStates: Int) extends AlogicAST
case class StateStmt(state: Int) extends AlogicAST // This is both added and removed by MakeStates
case class StateBlock(state: Int, contents: List[AlogicAST]) extends AlogicAST
case class GotoState(state: Int) extends AlogicAST

// AlogicType used to define the allowed types
sealed trait AlogicType
case class IntType(signed: Boolean, size: Int) extends AlogicType
case class IntVType(signed: Boolean, args: List[AlogicAST]) extends AlogicType // variable number of bits definition
case class Struct(fields: List[FieldType]) extends AlogicType
case object State extends AlogicType // Type with enough bits to hold state variable

// FieldType used for fields within a structure
sealed trait FieldType
case class Field(typ: AlogicType, name: String) extends FieldType

// SyncType for allowed port types
sealed trait SyncType
case object SyncReadyBubble extends SyncType
case object SyncReady extends SyncType
case object SyncAccept extends SyncType
case object Sync extends SyncType
case object WireSyncAccept extends SyncType
case object WireSync extends SyncType
case object Wire extends SyncType
