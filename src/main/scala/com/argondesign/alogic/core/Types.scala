////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Representations of various user facing and internal data types.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math

object Types {

  //////////////////////////////////////////////////////////////////////////////
  // Root trait of all types
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Type extends TypeOps

  //////////////////////////////////////////////////////////////////////////////
  // Fundamental types (those which can be the value of a type expression)
  //////////////////////////////////////////////////////////////////////////////

  sealed trait TypeFund extends Type

  // format: off
  sealed trait TypeInt extends TypeFund with TypeIntImpl
  case class TypeSInt(size: Long) extends TypeInt
  case class TypeUInt(size: Long) extends TypeInt
  case class TypeNum(signed: Boolean) extends TypeFund
  case class TypeVector(kind: TypeFund, size: Long) extends TypeFund with TypeVectorImpl
  case object TypeVoid extends TypeFund
  case object TypeStr extends TypeFund
  case class TypeRecord(symbol: Symbol, members: List[Symbol]) extends TypeFund with TypeCompound with TypeRecordImpl
  case class TypeEntity(symbol: Symbol, members: List[Symbol]) extends TypeFund with TypeCompound with TypeEntityImpl
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Types derived from fundamental types
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  case class TypeIn(kind: TypeFund, fc: FlowControlType) extends Type with TypeInImpl
  case class TypeOut(kind: TypeFund, fc: FlowControlType, st: StorageType) extends Type with TypeOutImpl
  case class TypePipeVar(kind: TypeFund) extends Type
  case class TypeParam(kind: TypeFund) extends Type
  case class TypeConst(kind: TypeFund) extends Type
  case class TypeGen(kind: TypeFund) extends Type
  case class TypeArray(kind: TypeFund, size: Long) extends Type with TypeArrayImpl
  case class TypeSram(kind: TypeFund, size: Long, st: StorageType) extends Type with TypeSramImpl
  case class TypeStack(kind: TypeFund, size: Long) extends Type with TypeStackImpl
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Type port types
  //////////////////////////////////////////////////////////////////////////////

  case class TypePipeIn(fc: FlowControlType) extends Type with TypePipeInImpl
  case class TypePipeOut(fc: FlowControlType, st: StorageType) extends Type with TypePipeOutImpl

  //////////////////////////////////////////////////////////////////////////////
  // Type wrappers for semantic disambiguation
  //////////////////////////////////////////////////////////////////////////////

  // Type of types, for example expressions that represent types like 'u8' or
  // 'bool[2]', and named type symbols e.g.: of 'foo' in 'fsm foo {}'
  case class TypeType(kind: TypeFund) extends Type

  // Type of expressions that are neither a term nor a type, e.g.: the type of
  // 'A.f' with definition "struct A {u8 f;}"
  case class TypeNone(kind: Type) extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Package type
  //////////////////////////////////////////////////////////////////////////////

  case class TypePackage(symbol: Symbol, members: List[Symbol])
      extends Type
      with TypeCompound
      with TypePackageImpl

  //////////////////////////////////////////////////////////////////////////////
  // Type of scopes
  //////////////////////////////////////////////////////////////////////////////

  case class TypeScope(symbol: Symbol, members: List[Symbol])
      extends Type
      with TypeCompound
      with TypeScopeImpl

  //////////////////////////////////////////////////////////////////////////////
  // Parametrized type
  //////////////////////////////////////////////////////////////////////////////

  case class TypeParametrized(symbol: Symbol) extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Types of statements
  //////////////////////////////////////////////////////////////////////////////

  case object TypeCombStmt extends Type
  case object TypeCtrlStmt extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Callable types
  //////////////////////////////////////////////////////////////////////////////

  sealed trait TypeCallable extends Type {
    val symbol: Symbol
    val retType: TypeFund
    val argTypes: List[Type]
  }

  sealed trait TypeMethod extends TypeCallable

  // format: off
  case class TypeCombFunc(symbol: Symbol, retType: TypeFund, argTypes: List[Type]) extends TypeCallable
  case class TypeCtrlFunc(symbol: Symbol, retType: TypeFund, argTypes: List[TypeFund]) extends TypeCallable
  case class TypeXenoFunc(symbol: Symbol, retType: TypeFund, argTypes: List[TypeFund]) extends TypeCallable
  case class TypeStaticMethod(symbol: Symbol, retType: TypeFund, argTypes: List[TypeFund]) extends TypeMethod
  case class TypeNormalMethod(symbol: Symbol, retType: TypeFund, argTypes: List[TypeFund]) extends TypeMethod
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Other miscellaneous types
  //////////////////////////////////////////////////////////////////////////////

  case class TypeState(symbol: Symbol) extends Type
  case object TypeMisc extends Type
  case object TypeBuiltin extends Type // type of naked builtin symbols, e.g. in: "@bits + 1"
  case object TypeError extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Base trait companions
  //////////////////////////////////////////////////////////////////////////////

  object TypeInt {

    def apply(signed: Boolean, size: Long): TypeInt = {
      if (signed) TypeSInt(size) else TypeUInt(size)
    }

    def unapply(kind: Type): Option[(Boolean, Long)] = kind match {
      case TypeSInt(size) => Some((true, size))
      case TypeUInt(size) => Some((false, size))
      case _              => None
    }

  }

  object TypeCallable {

    def unapply(kind: Type): Option[(Symbol, Type, List[Type])] = kind match {
      case callable: TypeCallable => Some((callable.symbol, callable.retType, callable.argTypes))
      case _                      => None
    }

  }

}

//////////////////////////////////////////////////////////////////////////////
// Implementations
//////////////////////////////////////////////////////////////////////////////

// A base trait for types that have fields that can be looked up using dot notation
trait TypeCompound {
  // List of public symbols
  def publicSymbols: List[Symbol]
  // 'apply' returns the selected symbol
  final def apply(sel: String): Option[Symbol] = publicSymbols.find(_.name == sel)
}

trait TypeIntImpl { this: TypeInt =>
  val size: Long
  require(size > 0)
}

trait TypeVectorImpl { this: TypeVector =>
  require(size > 0)
}

trait TypeRecordImpl { this: TypeRecord =>
  final def publicSymbols: List[Symbol] = members

  final lazy val dataMembers: List[Symbol] = members.filter(_.kind.isFund)
}

trait TypeEntityImpl { this: TypeEntity =>
  final def publicSymbols: List[Symbol] = members

  final lazy val portMembers: List[Symbol] = members filter {
    _.kind match {
      case _: TypeIn      => true
      case _: TypeOut     => true
      case _: TypePipeIn  => true
      case _: TypePipeOut => true
      case _              => false
    }
  }

}

trait TypePackageImpl { this: TypePackage =>
  final def publicSymbols: List[Symbol] = members
}

trait TypeScopeImpl { this: TypeScope =>
  final def publicSymbols: List[Symbol] = members

  final def scoped(name: String): Option[Symbol] =
    members.iterator
      .map { symbol =>
        (symbol, symbol.attr.localName.getOrElse(symbol.name))
      }
      .collectFirst {
        case (symbol, `name`) => symbol
      }

}

// Base trait of types that add fields to existing types
trait ExtensionType extends TypeCompound {
  // The underlying type
  def kind: Type

  // The extensions
  protected def extensionSymbols: List[Symbol]

  final lazy val publicSymbols: List[Symbol] = kind match {
    case ct: TypeCompound => extensionSymbols ::: ct.publicSymbols
    case _                => extensionSymbols
  }

}

trait TypeInImpl extends ExtensionType { this: TypeIn =>

  // TODO: fix symbol Ids, here and below
  final lazy val extensionSymbols: List[Symbol] = fc match {
    case FlowControlTypeNone =>
      val read = new Symbol("read")
      read.kind = TypeCombFunc(read, kind, Nil)
      List(read)
    case FlowControlTypeValid | FlowControlTypeReady =>
      val read = new Symbol("read")
      read.kind = TypeCombFunc(read, kind, Nil)
      val valid = new Symbol("valid")
      valid.kind = TypeUInt(1)
      List(read, valid)
  }

}

trait TypeOutImpl extends ExtensionType { this: TypeOut =>

  final lazy val extensionSymbols: List[Symbol] = {
    def writeFuncType(symbol: Symbol): TypeCombFunc = kind match {
      case TypeVoid => TypeCombFunc(symbol, TypeVoid, Nil)
      case _        => TypeCombFunc(symbol, TypeVoid, List(kind))
    }

    fc match {
      case FlowControlTypeNone =>
        val write = new Symbol("write")
        write.kind = writeFuncType(write)
        List(write)
      case FlowControlTypeValid =>
        val write = new Symbol("write")
        write.kind = writeFuncType(write)
        val valid = new Symbol("valid")
        valid.kind = TypeUInt(1)
        List(write, valid)
      case FlowControlTypeReady =>
        val write = new Symbol("write")
        write.kind = writeFuncType(write)
        val valid = new Symbol("valid")
        valid.kind = TypeUInt(1)
        val full = new Symbol("full")
        full.kind = TypeUInt(1)
        val empty = new Symbol("empty")
        empty.kind = TypeUInt(1)
        val space = new Symbol("space")
        val nSlices = st match {
          case StorageTypeSlices(slices) => slices.length
          case _                         => 1
        }
        space.kind = TypeUInt(nSlices)
        List(write, valid, full, empty, space)
    }
  }

}

trait TypePipeInImpl extends TypeCompound { this: TypePipeIn =>

  final lazy val publicSymbols: List[Symbol] = fc match {
    case FlowControlTypeNone =>
      val read = new Symbol("read")
      read.kind = TypeCombFunc(read, TypeVoid, Nil)
      List(read)
    case FlowControlTypeValid | FlowControlTypeReady =>
      val read = new Symbol("read")
      read.kind = TypeCombFunc(read, TypeVoid, Nil)
      val valid = new Symbol("valid")
      valid.kind = TypeUInt(1)
      List(read, valid)
  }

}

trait TypePipeOutImpl extends TypeCompound { this: TypePipeOut =>

  final lazy val publicSymbols: List[Symbol] = fc match {
    case FlowControlTypeNone =>
      val write = new Symbol("write")
      write.kind = TypeCombFunc(write, TypeVoid, Nil)
      List(write)
    case FlowControlTypeValid =>
      val write = new Symbol("write")
      write.kind = TypeCombFunc(write, TypeVoid, Nil)
      val valid = new Symbol("valid")
      valid.kind = TypeUInt(1)
      List(write, valid)
    case FlowControlTypeReady =>
      val write = new Symbol("write")
      write.kind = TypeCombFunc(write, TypeVoid, Nil)
      val valid = new Symbol("valid")
      valid.kind = TypeUInt(1)
      val full = new Symbol("full")
      full.kind = TypeUInt(1)
      val empty = new Symbol("empty")
      empty.kind = TypeUInt(1)
      val space = new Symbol("space")
      val nSlices = st match {
        case StorageTypeSlices(slices) => slices.length
        case _                         => 1
      }
      space.kind = TypeUInt(nSlices)
      List(write, valid, full, empty, space)
  }

}

trait TypeStackImpl extends TypeCompound { this: TypeStack =>
  require(size > 0)

  final lazy val publicSymbols: List[Symbol] = {
    val push = new Symbol("push")
    push.kind = TypeCombFunc(push, TypeVoid, Nil)
    val pop = new Symbol("pop")
    pop.kind = TypeCombFunc(pop, TypeVoid, Nil)
    val top = new Symbol("top")
    top.kind = kind
    val old = new Symbol("old")
    old.kind = kind
    List(push, pop, top, old)
  }

}

trait TypeArrayImpl extends TypeCompound { this: TypeArray =>
  require(size > 0)

  final private val _publicSymbols: List[Symbol] = {
    val addrType = TypeUInt(Math.clog2(size) max 1)
    val write = new Symbol("write")
    write.kind = TypeCombFunc(write, TypeVoid, List(addrType, kind))
    List(write)
  }

  final def publicSymbols: List[Symbol] = _publicSymbols
}

trait TypeSramImpl extends TypeCompound { this: TypeSram =>
  require(size > 0)

  final lazy val publicSymbols: List[Symbol] = {
    val addrType = TypeUInt(Math.clog2(size) max 1)
    val read = new Symbol("read")
    read.kind = TypeCombFunc(read, TypeVoid, List(addrType))
    val write = new Symbol("write")
    write.kind = TypeCombFunc(write, TypeVoid, List(addrType, kind))
    val rdata = new Symbol("rdata")
    rdata.kind = kind
    List(read, write, rdata)
  }

}
