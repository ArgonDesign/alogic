package alogic

// This file describes the classes used in the parser output.

// Case classes do not support inheritance
// Use sealed trait to force compiler to detect all cases are covered
// Use different abstract classes to decompose problem.

// // TaskType defines different types of modules
// sealed trait TaskType extends Positional
// case class Fsm() extends TaskType
// case class Pipeline() extends TaskType
// case class Network() extends TaskType
// case class Verilog() extends TaskType
// 
// // Declaration used for top-level and function declarations
// sealed trait Declaration extends Positional
// case class VarDeclaration(decltype: AlogicType,id: AlogicAST,init: Option[AlogicAST]) extends Declaration
// case class ConstDeclaration(decltype: AlogicType,id: AlogicAST,init: Option[AlogicAST]) extends Declaration
// case class VerilogDeclaration(decltype: AlogicType,id: AlogicAST) extends Declaration
// case class OutDeclaration(synctype: Option[SyncType],decltype: AlogicType,name: Name) extends Declaration
// case class InDeclaration(synctype: Option[SyncType],decltype: AlogicType,name: Name) extends Declaration
// 
// // TaskContent used for each function
// sealed trait TaskContent extends Positional
// case class Function(name: IDENTIFIER, body: AlogicAST) extends TaskContent
// case class FenceFunction(body: AlogicAST) extends TaskContent
// case class VerilogFunction(text:VERILOGSTMT) extends TaskContent
// 
// // AlogicAST used for abstract syntax nodes
sealed trait AlogicAST
case class Program(cmds : List[AlogicAST]) extends AlogicAST
// case class Num(value : String) extends AlogicAST  // Numbers held as textual representation
// case class Literal(value : LITERAL) extends AlogicAST  // Numbers held as textual representation
case class Name(value : String) extends AlogicAST
case class Define() extends AlogicAST
case class Typedef() extends AlogicAST
case class Task() extends AlogicAST
//case class Task(tasktype: TaskType, name: String, decls: List[Declaration], fns: List[TaskContent]) extends AlogicAST
// case class DottedName(names: List[IDENTIFIER]) extends AlogicAST
// case class ArrayLookup(name: AlogicAST, index: AlogicAST) extends AlogicAST
// case class BinaryArrayLookup(name: AlogicAST, lhs: AlogicAST, op: ArrayOp, rhs: AlogicAST) extends AlogicAST
// case class FunCall(name: DottedName, args: List[AlogicAST]) extends AlogicAST
// case class DollarFun(name: IDENTIFIER, args: List[AlogicAST]) extends AlogicAST
// case class ReadCall(name: DottedName, args: List[AlogicAST]) extends AlogicAST
// case class WriteCall(name: DottedName, args: List[AlogicAST]) extends AlogicAST
// case class Assign(lhs: AlogicAST, op: AlogicToken, rhs: AlogicAST) extends AlogicAST
// case class Plusplus(lhs: AlogicAST) extends AlogicAST
// case class Minusminus(lhs: AlogicAST) extends AlogicAST
// case class BinaryOp(lhs: AlogicAST, op: AlogicToken, rhs: AlogicAST) extends AlogicAST
// case class UnaryOp(op: AlogicToken, lhs: AlogicAST) extends AlogicAST
// case class Bracket(content: AlogicAST) extends AlogicAST
// case class TernaryOp(cond: AlogicAST, lhs: AlogicAST, rhs: AlogicAST) extends AlogicAST
// case class ControlBlock(cmds: List[AlogicAST]) extends AlogicAST
// case class CombinatorialBlock(cmds: List[AlogicAST]) extends AlogicAST
// case class FenceStmt() extends AlogicAST
// case class BreakStmt() extends AlogicAST
// case class ReturnStmt() extends AlogicAST
// case class GotoStmt(target:IDENTIFIER) extends AlogicAST
// case class DeclarationStmt(decl:VarDeclaration) extends AlogicAST  // Used when a declaration is mixed in with the code
// case class WhileLoop(cond:AlogicAST, body:AlogicAST) extends AlogicAST
// case class ControlFor(init:AlogicAST, cond:AlogicAST, incr:AlogicAST, body:List[AlogicAST]) extends AlogicAST
// case class ControlDo(cond:AlogicAST, body:List[AlogicAST]) extends AlogicAST
// case class CombinatorialIf(cond:AlogicAST, body:AlogicAST, elsebody:Option[AlogicAST]) extends AlogicAST
// case class ControlIf(cond:AlogicAST, body:AlogicAST, elsebody:Option[AlogicAST]) extends AlogicAST
// case class BitRep(count:AlogicAST,value:AlogicAST) extends AlogicAST
// case class BitCat(parts:List[AlogicAST]) extends AlogicAST
// case class AlogicComment(str:LITERAL) extends AlogicAST
// case class ControlCaseStmt(value:AlogicAST,cases:List[CaseLabel]) extends AlogicAST
// case class CombinatorialCaseStmt(value:AlogicAST,cases:List[CaseLabel]) extends AlogicAST
// 
// sealed trait CaseLabel extends Positional
// case class ControlCaseLabel(cond:List[AlogicAST],body: AlogicAST) extends CaseLabel
// case class CombinatorialCaseLabel(cond:List[AlogicAST],body: AlogicAST) extends CaseLabel
// 
// sealed trait ArrayOp extends Positional
// case class BitRange() extends ArrayOp
// case class LsbSelect() extends ArrayOp
// case class MsbSelect() extends ArrayOp
// 
// // AlogicType used to define the allowed types
sealed trait AlogicType
case class IntType(signed: Boolean, size : Int) extends AlogicType
case class IntVType(signed: Boolean, expr: AlogicAST) extends AlogicType // variable number of bits definition
// case class Struct(fields : List[FieldType]) extends AlogicType
case class State() extends AlogicType   // Type with enough bits to hold state variable
// 
// // FieldType used for fields within a structure
// sealed trait FieldType extends Positional
// case class Field(typ: AlogicType, name: IDENTIFIER) extends FieldType
// 
// // SyncType for allowed port types
// sealed trait SyncType extends Positional
// case class SyncReadyBubble() extends SyncType
// case class SyncReady() extends SyncType
// case class SyncAccept() extends SyncType
// case class Sync() extends SyncType
// case class WireSyncAccept() extends SyncType
// case class WireSync() extends SyncType
// case class Wire() extends SyncType
// 
// // SyncSpec provides a single word that is a sync type
// sealed trait SyncSpec extends Positional
// case class IdentWire() extends SyncSpec
// case class IdentSync() extends SyncSpec
// case class IdentBubble() extends SyncSpec
// case class IdentReady() extends SyncSpec
// case class IdentAccept() extends SyncSpec