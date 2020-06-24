////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2019 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.ast.Trees.Arg
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.util.CCLazy

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
  sealed trait TypeInt extends TypeFund
  case class TypeSInt(size: BigInt) extends TypeInt
  case class TypeUInt(size: BigInt) extends TypeInt
  case class TypeNum(signed: Boolean) extends TypeFund
  case class TypeVector(kind: TypeFund, size: BigInt) extends TypeFund
  case object TypeVoid extends TypeFund
  case object TypeStr extends TypeFund
  case class TypeRecord(symbol: Symbol, members: List[Symbol]) extends TypeFund with CompoundType with TypeRecordImpl
  case class TypeEntity(symbol: Symbol, ports: List[Symbol]) extends TypeFund with CompoundType with TypeEntityImpl
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Types derived from fundamental types
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  case class TypeIn(kind: TypeFund, fc: FlowControlType) extends Type with TypeInImpl
  case class TypeOut(kind: TypeFund, fc: FlowControlType, st: StorageType) extends Type with TypeOutImpl
  case class TypePipeline(kind: TypeFund) extends Type
  case class TypeParam(kind: TypeFund) extends Type
  case class TypeConst(kind: TypeFund) extends Type
  case class TypeGen(kind: TypeFund) extends Type
  case class TypeArray(kind: TypeFund, size: BigInt) extends Type with TypeArrayImpl
  case class TypeSram(kind: TypeFund, size: BigInt, st: StorageType) extends Type with TypeSramImpl
  case class TypeStack(kind: TypeFund, size: BigInt) extends Type with TypeStackImpl
  // format: on

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

  // This is callable, but is not quite a TypeCallable yet
  case class TypePolyFunc(symbol: Symbol, resolver: List[Arg] => Option[Symbol])
      extends Type
      with TypePolyFuncImpl

  //////////////////////////////////////////////////////////////////////////////
  // Unknown type (placeholder used prior to type checking)
  //////////////////////////////////////////////////////////////////////////////

  case object TypeUnknown extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Ambiguous type prior to Gen processing
  //////////////////////////////////////////////////////////////////////////////

  // TODO: delete
  case object TypeChoice extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Other miscellaneous types
  //////////////////////////////////////////////////////////////////////////////

  case class TypeState(symbol: Symbol) extends Type
  case object TypeMisc extends Type
  case object TypeError extends Type

  //////////////////////////////////////////////////////////////////////////////
  // Base trait companions
  //////////////////////////////////////////////////////////////////////////////

  object TypeInt {

    def apply(signed: Boolean, size: BigInt): TypeInt = {
      if (signed) TypeSInt(size) else TypeUInt(size)
    }

    def unapply(kind: Type): Option[(Boolean, BigInt)] = kind match {
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
trait CompoundType {
  // List of public symbols
  def publicSymbols(implicit cc: CompilerContext): List[Symbol]
  // Apply returns the selected symbol
  final def apply(sel: String)(implicit cc: CompilerContext): Option[Symbol] =
    publicSymbols find { _.name == sel }
}

trait TypeRecordImpl { this: TypeRecord =>
  final def dataMembers(implicit cc: CompilerContext): List[Symbol] = _dataMembers(cc)

  final private val _dataMembers = CCLazy[List[Symbol]] { implicit cc =>
    members filter { _.kind.isFund }
  }

  final def publicSymbols(implicit cc: CompilerContext): List[Symbol] = _publicSymbols(cc)

  final private val _publicSymbols = CCLazy[List[Symbol]] { _ =>
    // Doesn't depend on cc right now but will in the future
    members
  }

}

trait TypeEntityImpl { this: TypeEntity =>
  final def publicSymbols(implicit cc: CompilerContext): List[Symbol] = _publicSymbols(cc)

  final private val _publicSymbols = CCLazy[List[Symbol]] { _ =>
    // Doesn't depend on cc right now but will in the future
    ports
  }

}

// Base trait of types that add fields to existing types
trait ExtensionType extends CompoundType {
  // The underlying type
  def kind: Type

  // The extensions
  protected def extensionSymbols: List[Symbol]

  final def publicSymbols(implicit cc: CompilerContext): List[Symbol] = _publicSymbols(cc)

  final private val _publicSymbols = CCLazy[List[Symbol]] { implicit cc =>
    kind match {
      case ct: CompoundType => extensionSymbols ::: ct.publicSymbols
      case _                => extensionSymbols
    }
  }

}

trait TypeInImpl extends ExtensionType { this: TypeIn =>

  final protected def extensionSymbols: List[Symbol] = fc match {
    case FlowControlTypeNone =>
      val read = new Symbol(-1, Loc.synthetic, "read")
      read.kind = TypeCombFunc(read, kind, Nil)
      List(read)
    case FlowControlTypeValid | FlowControlTypeReady =>
      val read = new Symbol(-1, Loc.synthetic, "read")
      read.kind = TypeCombFunc(read, kind, Nil)
      val valid = new Symbol(-1, Loc.synthetic, "valid")
      valid.kind = TypeUInt(1)
      List(read, valid)
  }

}

trait TypeOutImpl extends ExtensionType { this: TypeOut =>

  final protected def extensionSymbols: List[Symbol] = {
    def writeFuncType(symbol: Symbol): TypeCombFunc = kind match {
      case TypeVoid => TypeCombFunc(symbol, TypeVoid, Nil)
      case _        => TypeCombFunc(symbol, TypeVoid, List(kind))
    }

    fc match {
      case FlowControlTypeNone =>
        val write = new Symbol(-1, Loc.synthetic, "write")
        write.kind = writeFuncType(write)
        List(write)
      case FlowControlTypeValid =>
        val write = new Symbol(-1, Loc.synthetic, "write")
        write.kind = writeFuncType(write)
        val valid = new Symbol(-1, Loc.synthetic, "valid")
        valid.kind = TypeUInt(1)
        List(write, valid)
      case FlowControlTypeReady =>
        val write = new Symbol(-1, Loc.synthetic, "write")
        write.kind = writeFuncType(write)
        val valid = new Symbol(-1, Loc.synthetic, "valid")
        valid.kind = TypeUInt(1)
        val full = new Symbol(-1, Loc.synthetic, "full")
        full.kind = TypeUInt(1)
        val empty = new Symbol(-1, Loc.synthetic, "empty")
        empty.kind = TypeUInt(1)
        val space = new Symbol(-1, Loc.synthetic, "space")
        val nSlices = st match {
          case StorageTypeSlices(slices) => slices.length
          case _                         => 1
        }
        space.kind = TypeUInt(nSlices)
        List(write, valid, full, empty, space)
    }
  }

}

trait TypeStackImpl extends CompoundType { this: TypeStack =>

  final private val _publicSymbols: List[Symbol] = {
    val push = new Symbol(-1, Loc.synthetic, "push")
    push.kind = TypeCombFunc(push, TypeVoid, List(kind))
    val pop = new Symbol(-1, Loc.synthetic, "pop")
    pop.kind = TypeCombFunc(pop, kind, Nil)
    val set = new Symbol(-1, Loc.synthetic, "set")
    set.kind = TypeCombFunc(set, TypeVoid, List(kind))
    val top = new Symbol(-1, Loc.synthetic, "top")
    top.kind = kind
    val full = new Symbol(-1, Loc.synthetic, "full")
    full.kind = TypeUInt(1)
    val empty = new Symbol(-1, Loc.synthetic, "empty")
    empty.kind = TypeUInt(1)
    List(push, pop, set, top, full, empty)
  }

  final def publicSymbols(implicit cc: CompilerContext): List[Symbol] = _publicSymbols
}

trait TypeArrayImpl extends CompoundType { this: TypeArray =>

  final private val _publicSymbols: List[Symbol] = {
    val write = new Symbol(-1, Loc.synthetic, "write")
    write.kind = TypeCombFunc(write, TypeVoid, List(TypeUInt(Math.clog2(size)), kind))
    List(write)
  }

  final def publicSymbols(implicit cc: CompilerContext): List[Symbol] = _publicSymbols
}

trait TypeSramImpl extends CompoundType { this: TypeSram =>

  final private val _publicSymbols: List[Symbol] = {
    val addrType = TypeUInt(Math.clog2(size))
    val read = new Symbol(-1, Loc.synthetic, "read")
    read.kind = TypeCombFunc(read, TypeVoid, List(addrType))
    val write = new Symbol(-1, Loc.synthetic, "write")
    write.kind = TypeCombFunc(write, TypeVoid, List(addrType, kind))
    val rdata = new Symbol(-1, Loc.synthetic, "rdata")
    rdata.kind = kind
    List(read, write, rdata)
  }

  final def publicSymbols(implicit cc: CompilerContext): List[Symbol] = _publicSymbols
}

trait TypePolyFuncImpl { this: TypePolyFunc =>
  // Resolve based on arguments to the correct overload of the symbol
  final def resolve(args: List[Arg]): Option[Symbol] = resolver(args)
}
