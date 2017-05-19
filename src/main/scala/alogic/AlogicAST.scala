package alogic

// This file describes the classes used in the parser output.

// Case classes do not support inheritance
// Use sealed trait to force compiler to detect all cases are covered
// Use different abstract classes to decompose problem.

// // TaskType defines different types of modules
sealed trait TaskType
case class Fsm() extends TaskType
case class Pipeline() extends TaskType
case class Network() extends TaskType
case class Verilog() extends TaskType
//
// // Declaration used for top-level and function declarations
sealed trait Declaration
case class VarDeclaration(decltype: AlogicType, id: AlogicAST, init: Option[AlogicAST]) extends Declaration
case class ParamDeclaration(decltype: AlogicType, id: String, init: Option[AlogicAST]) extends Declaration
case class VerilogDeclaration(decltype: AlogicType, id: AlogicAST) extends Declaration
case class OutDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration
case class InDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration

// // AlogicAST used for abstract syntax nodes
sealed trait AlogicAST
case class Connect(start: AlogicAST, end: AlogicAST) extends AlogicAST
case class Instantiate(id: String, module: String, args: List[AlogicAST]) extends AlogicAST
case class Function(name: String, body: AlogicAST) extends AlogicAST
case class FenceFunction(body: AlogicAST) extends AlogicAST
case class VerilogFunction(body: String) extends AlogicAST
case class Num(value: String) extends AlogicAST // Numbers held as textual representation
case class Literal(value: String) extends AlogicAST // Strings held as textual representation including quotes
case class Task(tasktype: TaskType, name: String, decls: List[Declaration], fns: List[AlogicAST]) extends AlogicAST
case class DottedName(names: List[String]) extends AlogicAST
case class ArrayLookup(name: AlogicAST, index: AlogicAST) extends AlogicAST
case class BinaryArrayLookup(name: AlogicAST, lhs: AlogicAST, op: String, rhs: AlogicAST) extends AlogicAST
case class FunCall(name: AlogicAST, args: List[AlogicAST]) extends AlogicAST
case class Zxt(numbits: AlogicAST, expr: AlogicAST) extends AlogicAST
case class Sxt(numbits: AlogicAST, expr: AlogicAST) extends AlogicAST
case class DollarCall(name: String, args: List[AlogicAST]) extends AlogicAST
case class ReadCall(name: DottedName) extends AlogicAST
case class LockCall(name: DottedName) extends AlogicAST
case class UnlockCall(name: DottedName) extends AlogicAST
case class ValidCall(name: DottedName) extends AlogicAST
case class WriteCall(name: DottedName, args: List[AlogicAST]) extends AlogicAST
case class Assign(lhs: AlogicAST, op: String, rhs: AlogicAST) extends AlogicAST
case class BinaryOp(lhs: AlogicAST, op: String, rhs: AlogicAST) extends AlogicAST
case class UnaryOp(op: String, lhs: AlogicAST) extends AlogicAST
case class Bracket(content: AlogicAST) extends AlogicAST
case class TernaryOp(cond: AlogicAST, lhs: AlogicAST, rhs: AlogicAST) extends AlogicAST
case class CombinatorialBlock(cmds: List[AlogicAST]) extends AlogicAST
case class DeclarationStmt(decl: VarDeclaration) extends AlogicAST // Used when a declaration is mixed in with the code
case class CombinatorialIf(cond: AlogicAST, body: AlogicAST, elsebody: Option[AlogicAST]) extends AlogicAST
case class BitRep(count: AlogicAST, value: AlogicAST) extends AlogicAST
case class BitCat(parts: List[AlogicAST]) extends AlogicAST
case class AlogicComment(str: String) extends AlogicAST
case class CombinatorialCaseStmt(value: AlogicAST, cases: List[AlogicAST]) extends AlogicAST
case class CombinatorialCaseLabel(cond: List[AlogicAST], body: AlogicAST) extends AlogicAST

// Types removed by AstBuilder
case class Define() extends AlogicAST
case class Typedef() extends AlogicAST

// Types removed by Desugar (also += style operations)
case class Plusplus(lhs: AlogicAST) extends AlogicAST
case class Minusminus(lhs: AlogicAST) extends AlogicAST

// Types removed by MakeStates
case class Program(cmds: List[AlogicAST]) extends AlogicAST
case class ControlCaseStmt(value: AlogicAST, cases: List[AlogicAST]) extends AlogicAST
case class ControlIf(cond: AlogicAST, body: AlogicAST, elsebody: Option[AlogicAST]) extends AlogicAST
case class ControlBlock(cmds: List[AlogicAST]) extends AlogicAST
case class WhileLoop(cond: AlogicAST, body: AlogicAST) extends AlogicAST
case class ControlFor(init: AlogicAST, cond: AlogicAST, incr: AlogicAST, body: List[AlogicAST]) extends AlogicAST
case class ControlDo(cond: AlogicAST, body: List[AlogicAST]) extends AlogicAST
case class FenceStmt() extends AlogicAST
case class BreakStmt() extends AlogicAST
case class ReturnStmt() extends AlogicAST
case class GotoStmt(target: String) extends AlogicAST
case class ControlCaseLabel(cond: List[AlogicAST], body: AlogicAST) extends AlogicAST

// Extra types inserted by MakeStates
case class StateProgram(cmds: List[AlogicAST], numStates: Int) extends AlogicAST
case class StateStmt(state: Int) extends AlogicAST
case class GotoState(state: Int) extends AlogicAST

// AlogicType used to define the allowed types
sealed trait AlogicType
case class IntType(signed: Boolean, size: Int) extends AlogicType
case class IntVType(signed: Boolean, args: List[AlogicAST]) extends AlogicType // variable number of bits definition
case class Struct(fields: List[FieldType]) extends AlogicType
case class State() extends AlogicType // Type with enough bits to hold state variable

// FieldType used for fields within a structure
sealed trait FieldType
case class Field(typ: AlogicType, name: String) extends FieldType

// SyncType for allowed port types
sealed trait SyncType
case class SyncReadyBubble() extends SyncType
case class SyncReady() extends SyncType
case class SyncAccept() extends SyncType
case class Sync() extends SyncType
case class WireSyncAccept() extends SyncType
case class WireSync() extends SyncType
case class Wire() extends SyncType

