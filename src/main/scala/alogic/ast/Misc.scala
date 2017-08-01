////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

// Various representational case classes not descendant from ast.Node

// // Declaration used for top-level and function declarations
sealed trait Declaration extends DeclarationOps

object Declaration {
  def unapply(decl: Declaration) = Some((decl.decltype, decl.id))
}

case class VarDeclaration(decltype: Type, id: String, init: Option[Expr]) extends Declaration
case class ArrayDeclaration(decltype: ScalarType, id: String, dims: List[Expr]) extends Declaration
case class PipelineVarDeclaration(decltype: Type, id: String) extends Declaration
case class ParamDeclaration(decltype: ScalarType, id: String, init: Expr) extends Declaration
case class ConstDeclaration(decltype: ScalarType, id: String, init: Expr) extends Declaration
case class OutDeclaration(synctype: SyncType, decltype: Type, id: String) extends Declaration
case class InDeclaration(synctype: SyncType, decltype: Type, id: String) extends Declaration

sealed trait VerilogDeclaration extends Declaration
case class VerilogVarDeclaration(decltype: Type, id: String) extends VerilogDeclaration
case class VerilogArrayDeclaration(decltype: ScalarType, id: String, dims: List[Expr]) extends VerilogDeclaration

// AlogicType used to define the allowed types
sealed trait Type extends TypeOps
sealed trait ScalarType extends Type
case class IntType(signed: Boolean, size: Int) extends ScalarType
case class IntVType(signed: Boolean, args: List[Expr]) extends ScalarType // variable number of bits definition
case class Struct(name: String, fields: Map[String, Type]) extends Type

// SyncType for allowed port types
// TODO: separate the flow control (none/valid/ready/accept) type from the storage type (wire/bubble/reg)
sealed trait SyncType extends SyncTypeOps
case object SyncReadyBubble extends SyncType
case object SyncReady extends SyncType
case object SyncAccept extends SyncType
case object Sync extends SyncType
case object WireSyncAccept extends SyncType
case object WireSync extends SyncType
case object Wire extends SyncType
