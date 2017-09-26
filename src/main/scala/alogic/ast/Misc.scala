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

// Decl used for top-level and function declarations
sealed trait Decl extends DeclOps

object Decl {
  def unapply(decl: Decl) = Some((decl.kind, decl.id))
}

case class DeclVar(kind: Type, id: String, init: Option[Expr]) extends Decl
case class DeclArr(kind: ScalarType, id: String, dims: List[Expr]) extends Decl
case class DeclPippeVar(kind: Type, id: String) extends Decl
case class DeclParam(kind: ScalarType, id: String, init: Expr) extends Decl
case class DeclConst(kind: ScalarType, id: String, init: Expr) extends Decl
case class DeclIn(kind: Type, id: String, fctype: FlowControlType) extends Decl
case class DeclOut(kind: Type, id: String, fctype: FlowControlType, stype: StorageType) extends Decl

sealed trait DeclVerilog extends Decl
case class DeclVerilogVar(kind: Type, id: String) extends DeclVerilog
case class DeclVerilogArr(kind: ScalarType, id: String, dims: List[Expr]) extends DeclVerilog

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
