package alogic.ast

// Various representational case classes not descendant from ast.Node

// // Declaration used for top-level and function declarations
sealed trait Declaration
// TODO: Add separate Decl for Arrays, it just makes a lot of things simpler
case class VarDeclaration(decltype: AlogicType, id: AlogicAST, init: Option[AlogicExpr]) extends Declaration
case class ParamDeclaration(decltype: AlogicType, id: String, init: AlogicExpr) extends Declaration
case class ConstDeclaration(decltype: AlogicType, id: String, init: AlogicExpr) extends Declaration
case class VerilogDeclaration(decltype: AlogicType, id: AlogicAST) extends Declaration
case class OutDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration
case class InDeclaration(synctype: SyncType, decltype: AlogicType, name: String) extends Declaration

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
