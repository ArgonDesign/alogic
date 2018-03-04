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
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.lib.StructuredTree

import FlowControlTypes.FlowControlType
import FlowControlTypes.FlowControlTypeAccept
import FlowControlTypes.FlowControlTypeReady
import FlowControlTypes.FlowControlTypeValid
import StorageTypes.StorageType

object Types {

  // The type system represents more than just  user facing types.
  // Inside the compiler, every tree node has a type.

  // Root of all types
  sealed trait Type extends StructuredTree with TypeOps

  ///////////////////////////////////////////////////////////////////////////////
  // All possible types
  ///////////////////////////////////////////////////////////////////////////////

  // Type of expressions that represent types e.g. ExprType(_)
  case object TypeType extends Type
  // Type of combinatorial statements
  case object TypeComb extends Type
  // Type of control statements
  case object TypeCtrl extends Type

  // Simple integer types e.g.: i8 / u2 / int(N), analogous to Verilog packed arrays
  case class TypeInt(signed: Boolean, size: Expr) extends Type
  // Vector types (analogous to higher dimensions of SystemVerilog multi-dimensional packed arrays)
  case class TypeVector(elementType: Type, size: Expr) extends Type
  // Array types (analogous to verilog unpacked arrays)
  case class TypeArray(elementType: Type, size: Expr) extends Type
  // Structure type
  case class TypeStruct(fieldNames: List[String], fieldTypes: List[Type]) extends Type with TypeStructImpl
  // Void type
  case object TypeVoid extends Type
  // Type reference e.g. 'foo_t foo;'
  case class TypeRef(ref: Ref) extends Type
  // Function type e.g. 'void foo() {}'
  case class TypeFunc(argTypes: List[Type], retType: Type) extends Type
  // Entity type e.g. 'fsm foo {}'
  case class TypeEntity(
    portNames:  List[String],
    portTypes:  List[Type],
    paramNames: List[String],
    paramTypes: List[Type]
  ) extends Type with TypeEntityImpl

  // Input port type
  case class TypeIn(kind: Type, fct: FlowControlType) extends Type with TypeInImpl
  // Output port type
  case class TypeOut(kind: Type, fct: FlowControlType, st: StorageType) extends Type with TypeOutImpl
  // Pipeline variable type
  case class TypePipeline(kind: Type) extends Type
  // Parameter type
  case class TypeParam(kind: Type) extends Type
  // Constant type
  case class TypeConst(kind: Type) extends Type

  ///////////////////////////////////////////////////////////////////////////////
  // Implementations
  ///////////////////////////////////////////////////////////////////////////////

  // A base trait for types that have fields that can be looked up using dot notation
  trait CompoundType {
    // Apply returns the type of the field selected
    def apply(field: String): Option[Type]
  }

  trait TypeStructImpl extends CompoundType { this: TypeStruct =>

    private[this] lazy val fieldMap = (fieldNames zip fieldTypes).toMap

    def apply(name: String) = fieldMap.get(name)
  }

  trait TypeEntityImpl extends CompoundType { this: TypeEntity =>

    private[this] lazy val portMap = (portNames zip portTypes).toMap

    private[this] lazy val paramMap = (paramNames zip paramTypes).toMap

    def apply(name: String) = portMap.get(name)

    def param(name: String) = paramMap.get(name)
  }

  trait TypeInImpl extends CompoundType { this: TypeIn =>

    lazy val fieldMap = fct match {
      case FlowControlTypeNone => {
        Map.empty[String, Type]
      }
      case FlowControlTypeValid | FlowControlTypeReady => {
        Map(
          "read" -> TypeFunc(Nil, kind),
          "valid" -> TypeFunc(Nil, TypeInt(false, Expr(1))),
          "wait" -> TypeFunc(Nil, TypeVoid)
        )
      }
      case FlowControlTypeAccept => {
        Map(
          "read" -> TypeFunc(Nil, kind)
        )
      }
    }

    def apply(name: String) = fieldMap.get(name)
  }

  trait TypeOutImpl extends CompoundType { this: TypeOut =>

    lazy val fieldMap = {
      val writeFuncType = if (kind == TypeVoid) TypeFunc(Nil, TypeVoid) else TypeFunc(List(kind), TypeVoid)
      fct match {
        case FlowControlTypeNone => {
          Map.empty[String, Type]
        }
        case FlowControlTypeValid => {
          Map(
            "write" -> writeFuncType,
            "valid" -> TypeFunc(Nil, TypeInt(false, Expr(1))),
            "flush" -> TypeFunc(Nil, TypeVoid)
          )
        }
        case FlowControlTypeReady => {
          Map(
            "write" -> writeFuncType,
            "valid" -> TypeFunc(Nil, TypeInt(false, Expr(1))),
            "flush" -> TypeFunc(Nil, TypeVoid),
            "full" -> TypeFunc(Nil, TypeInt(false, Expr(1))),
            "empty" -> TypeFunc(Nil, TypeInt(false, Expr(1)))
          )
        }
        case FlowControlTypeAccept => {
          Map(
            "write" -> writeFuncType,
          )
        }
      }
    }

    def apply(name: String) = fieldMap.get(name)
  }
}
