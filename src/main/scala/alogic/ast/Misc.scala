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
case class InDeclaration(fctype: FlowControlType, decltype: Type, id: String) extends Declaration
case class OutDeclaration(fctype: FlowControlType, decltype: Type, id: String, stype: StorageType) extends Declaration

sealed trait VerilogDeclaration extends Declaration
case class VerilogVarDeclaration(decltype: Type, id: String) extends VerilogDeclaration
case class VerilogArrayDeclaration(decltype: ScalarType, id: String, dims: List[Expr]) extends VerilogDeclaration

// AlogicType used to define the allowed types
sealed trait Type extends TypeOps
sealed trait ScalarType extends Type {
  val signed: Boolean
}
case class IntType(signed: Boolean, size: Int) extends ScalarType
case class IntVType(signed: Boolean, args: List[Expr]) extends ScalarType // variable number of bits definition
case class Struct(name: String, fields: Map[String, Type]) extends Type
case object VoidType extends Type // No bits, just flow control signals

sealed trait FlowControlType extends FlowControlTypeOps
case object FlowControlTypeNone extends FlowControlType
case object FlowControlTypeValid extends FlowControlType
case object FlowControlTypeReady extends FlowControlType
case object FlowControlTypeAccept extends FlowControlType

sealed trait StorageType extends StorageTypeOps
case object StorageTypeWire extends StorageType
case object StorageTypeBubble extends StorageType
case object StorageTypeReg extends StorageType
