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
// Representations of various Alogic types
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import scala.collection.immutable.ListMap

import com.argondesign.alogic.ast.Trees.Expr

import FlowControlTypes.FlowControlType
import StorageTypes.StorageType
import Symbols.TypeSymbol

object Types {

  // Root of all types
  abstract sealed trait Type

  ///////////////////////////////////////////////////////////////////////////////
  // Basic type
  ///////////////////////////////////////////////////////////////////////////////

  // Simple integer types e.g.: i8 / u2 / int(N), analogous to Verilog packed arrays
  case class TypeInt(signed: Boolean, size: Expr) extends Type
  // Vector types (analogous to higher dimensions of SystemVerilog multi-dimensional packed arrays)
  case class TypeVector(elementType: Type, size: Expr) extends Type
  // Array types (analogous to verilog unpacked arrays)
  case class TypeArray(elementType: Type, size: Expr) extends Type
  // Structure type
  case class TypeStruct(symbol: TypeSymbol, fields: ListMap[String, Type]) extends Type
  // Void type
  case object TypeVoid extends Type

  ///////////////////////////////////////////////////////////////////////////////
  // Qualified types with underlying types 'kind'
  ///////////////////////////////////////////////////////////////////////////////

  // Input port type
  case class TypeIn(kind: Type, fct: FlowControlType) extends Type
  // Output port type
  case class TypeOut(kind: Type, fct: FlowControlType, storage: StorageType) extends Type
  // Pipeline variable type
  case class TypePipeline(kind: Type) extends Type
  // Parameter type
  case class TypeParam(kind: Type) extends Type
  // Constant type
  case class TypeConst(kind: Type) extends Type
  // Verilog type TODO: review use of this
  case class TypeVerilog(kind: Type) extends Type

  ///////////////////////////////////////////////////////////////////////////////
  // Named type
  ///////////////////////////////////////////////////////////////////////////////

  // Named type (e.g.: foo_t a)
  case class TypeIdent(ident: String) extends Type
}
