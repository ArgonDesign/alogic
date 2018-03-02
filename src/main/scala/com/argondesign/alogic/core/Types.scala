////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Representations of various internal types.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._

import FlowControlTypes.FlowControlType
import StorageTypes.StorageType

object Types {

  // The type system represents more than just  user facing types.
  // Inside the compiler, every tree node has a type.

  // Root of all types
  sealed trait Type extends TreeLike with TypeOps

  // We have 2 basic kinds of types. GroundType are proper new types, wile
  // ProxyTypes refer to underlying types but attach further semantics

  sealed trait GroundType extends Type
  sealed trait ProxyType extends Type

  ///////////////////////////////////////////////////////////////////////////////
  // Ground types
  ///////////////////////////////////////////////////////////////////////////////

  // Simple integer types e.g.: i8 / u2 / int(N), analogous to Verilog packed arrays
  case class TypeInt(signed: Boolean, size: Expr) extends GroundType
  // Vector types (analogous to higher dimensions of SystemVerilog multi-dimensional packed arrays)
  case class TypeVector(elementType: Type, size: Expr) extends GroundType
  // Array types (analogous to verilog unpacked arrays)
  case class TypeArray(elementType: Type, size: Expr) extends GroundType
  // Structure type
  case class TypeStruct(fieldNames: List[String], fieldTypes: List[Type]) extends GroundType
  // Void type
  case object TypeVoid extends GroundType
  // Type reference e.g. 'foo_t foo;'
  case class TypeRef(ref: Ref) extends GroundType
  // Function type e.g. 'void foo() {}'
  case object TypeFunc extends GroundType
  // Entity type e.g. 'fsm foo {}'
  // TODO: add fields (ports)
  case object TypeEntity extends GroundType

  ///////////////////////////////////////////////////////////////////////////////
  // Proxy types with underlying types 'kind'
  ///////////////////////////////////////////////////////////////////////////////

  // Input port type
  case class TypeIn(kind: Type, fct: FlowControlType) extends ProxyType
  // Output port type
  case class TypeOut(kind: Type, fct: FlowControlType, storage: StorageType) extends ProxyType
  // Pipeline variable type
  case class TypePipeline(kind: Type) extends ProxyType
  // Parameter type
  case class TypeParam(kind: Type) extends ProxyType
  // Constant type
  case class TypeConst(kind: Type) extends ProxyType
}
