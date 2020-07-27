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
// Common members of Types.Type
// These are factored out into a separate file to keep Type.Types readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

trait TypeOps extends TypePrintOps { this: Type =>

  //////////////////////////////////////////////////////////////////////////////
  // Primitive type tests
  //////////////////////////////////////////////////////////////////////////////

  // $COVERAGE-OFF$ Trivial to keep full, but not necessarily used
  final def isFund: Boolean = this.isInstanceOf[TypeFund]
  final def isInt: Boolean = this.isInstanceOf[TypeInt]
  final def isSInt: Boolean = this.isInstanceOf[TypeSInt]
  final def isUInt: Boolean = this.isInstanceOf[TypeUInt]
  final def isNum: Boolean = this.isInstanceOf[TypeNum]
  final def isVector: Boolean = this.isInstanceOf[TypeVector]
  final def isVoid: Boolean = this eq TypeVoid
  final def isStr: Boolean = this eq TypeStr
  final def isRecord: Boolean = this.isInstanceOf[TypeRecord]
  final def isEntity: Boolean = this.isInstanceOf[TypeEntity]
  final def isIn: Boolean = this.isInstanceOf[TypeIn]
  final def isOut: Boolean = this.isInstanceOf[TypeOut]
  final def isPipeVar: Boolean = this.isInstanceOf[TypePipeVar]
  final def isPipeIn: Boolean = this.isInstanceOf[TypePipeIn]
  final def isPipeOut: Boolean = this.isInstanceOf[TypePipeOut]
  final def isParam: Boolean = this.isInstanceOf[TypeParam]
  final def isConst: Boolean = this.isInstanceOf[TypeConst]
  final def isGen: Boolean = this.isInstanceOf[TypeGen]
  final def isArray: Boolean = this.isInstanceOf[TypeArray]
  final def isSram: Boolean = this.isInstanceOf[TypeSram]
  final def isStack: Boolean = this.isInstanceOf[TypeStack]
  final def isType: Boolean = this.isInstanceOf[TypeType]
  final def isNone: Boolean = this.isInstanceOf[TypeNone]
  final def isPackage: Boolean = this.isInstanceOf[TypePackage]
  final def isParametrized: Boolean = this.isInstanceOf[TypeParametrized]
  final def isCombStmt: Boolean = this eq TypeCombStmt
  final def isCtrlStmt: Boolean = this eq TypeCtrlStmt
  final def isCallable: Boolean = this.isInstanceOf[TypeCallable]
  final def isCombFunc: Boolean = this.isInstanceOf[TypeCombFunc]
  final def isCtrlFunc: Boolean = this.isInstanceOf[TypeCtrlFunc]
  final def isPolyFunc: Boolean = this.isInstanceOf[TypePolyFunc]
  final def isXenoFunc: Boolean = this.isInstanceOf[TypeXenoFunc]
  final def isMethod: Boolean = this.isInstanceOf[TypeMethod]
  final def isStaticMethod: Boolean = this.isInstanceOf[TypeStaticMethod]
  final def isNormalMethod: Boolean = this.isInstanceOf[TypeNormalMethod]
  final def isState: Boolean = this.isInstanceOf[TypeState]
  final def isMisc: Boolean = this eq TypeMisc
  final def isError: Boolean = this eq TypeError
  final def isCompound: Boolean = this.isInstanceOf[TypeCompound]
  final def isScope: Boolean = this.isInstanceOf[TypeScope]
  // $COVERAGE-ON$

  //////////////////////////////////////////////////////////////////////////////
  // ... equivalent casts (unless the type is an object)
  //////////////////////////////////////////////////////////////////////////////

  // $COVERAGE-OFF$ Trivial to keep full, but not necessarily used
  final def asFund: TypeFund = this.asInstanceOf[TypeFund]
  final def asInt: TypeInt = this.asInstanceOf[TypeInt]
  final def asSInt: TypeSInt = this.asInstanceOf[TypeSInt]
  final def asUInt: TypeUInt = this.asInstanceOf[TypeUInt]
  final def asNum: TypeNum = this.asInstanceOf[TypeNum]
  final def asVector: TypeVector = this.asInstanceOf[TypeVector]
  final def asRecord: TypeRecord = this.asInstanceOf[TypeRecord]
  final def asEntity: TypeEntity = this.asInstanceOf[TypeEntity]
  final def asIn: TypeIn = this.asInstanceOf[TypeIn]
  final def asOut: TypeOut = this.asInstanceOf[TypeOut]
  final def asPipeVar: TypePipeVar = this.asInstanceOf[TypePipeVar]
  final def asPipeIn: TypePipeIn = this.asInstanceOf[TypePipeIn]
  final def asPipeOut: TypePipeOut = this.asInstanceOf[TypePipeOut]
  final def asParam: TypeParam = this.asInstanceOf[TypeParam]
  final def asConst: TypeConst = this.asInstanceOf[TypeConst]
  final def asGen: TypeGen = this.asInstanceOf[TypeGen]
  final def asArray: TypeArray = this.asInstanceOf[TypeArray]
  final def asSram: TypeSram = this.asInstanceOf[TypeSram]
  final def asStack: TypeStack = this.asInstanceOf[TypeStack]
  final def asType: TypeType = this.asInstanceOf[TypeType]
  final def asNone: TypeNone = this.asInstanceOf[TypeNone]
  final def asPackage: TypePackage = this.asInstanceOf[TypePackage]
  final def asParametrized: TypeParametrized = this.asInstanceOf[TypeParametrized]
  final def asCallable: TypeCallable = this.asInstanceOf[TypeCallable]
  final def asCombFunc: TypeCombFunc = this.asInstanceOf[TypeCombFunc]
  final def asCtrlFunc: TypeCtrlFunc = this.asInstanceOf[TypeCtrlFunc]
  final def asPolyFunc: TypePolyFunc = this.asInstanceOf[TypePolyFunc]
  final def asXenoFunc: TypeXenoFunc = this.asInstanceOf[TypeXenoFunc]
  final def asMethod: TypeMethod = this.asInstanceOf[TypeMethod]
  final def asStaticMethod: TypeStaticMethod = this.asInstanceOf[TypeStaticMethod]
  final def asNormalMethod: TypeNormalMethod = this.asInstanceOf[TypeNormalMethod]
  final def asState: TypeState = this.asInstanceOf[TypeState]
  final def asCompound: TypeCompound = this.asInstanceOf[TypeCompound]
  final def asScope: TypeScope = this.asInstanceOf[TypeScope]
  // $COVERAGE-ON$

  // Is this a primitive numeric type
  final lazy val isNumeric: Boolean = underlying match {
    case _: TypeInt => true
    case _: TypeNum => true
    case _          => false
  }

  // Is this a 'packed' type, i.e.: does it have a finite,
  // possibly 0 width bit-vector representation?
  final lazy val isPacked: Boolean = underlying match {
    case _: TypeSInt   => true
    case _: TypeUInt   => true
    case _: TypeRecord => true
    case _: TypeVector => true
    case TypeVoid      => true // TODO: should void be packed?
    case _             => false
  }

  // Signedness of this type (as far as expressions are concerned)
  final lazy val isSigned: Boolean = underlying match {
    case _: TypeSInt     => true
    case TypeNum(signed) => signed
    case _               => false
  }

  // Width of this type
  final lazy val width: BigInt = underlying match {
    case TypeSInt(size)          => size
    case TypeUInt(size)          => size
    case self: TypeRecord        => self.dataMembers.foldLeft(BigInt(0))(_ + _.kind.width)
    case TypeVoid                => 0
    case TypeVector(eType, size) => size * eType.width
    case _                       => unreachable
  }

  final def shapeIter: Iterator[BigInt] = underlying match {
    case TypeArray(elemKind, size)               => Iterator.single(size) ++ elemKind.shapeIter
    case TypeVector(elemKind, size)              => Iterator.single(size) ++ elemKind.shapeIter
    case kind if kind.isPacked && kind.width > 0 => Iterator.single(kind.width)
    case _                                       => Iterator.empty
  }

  // If this is a proxy type, get the underlying type, otherwise get this type
  final lazy val underlying: Type = this match {
    case TypeIn(kind, _)     => kind
    case TypeOut(kind, _, _) => kind
    case TypePipeVar(kind)   => kind
    case TypeParam(kind)     => kind
    case TypeConst(kind)     => kind
    case TypeGen(kind)       => kind
    case other               => other
  }

  def addVectorDimension(size: BigInt): TypeVector = this match {
    case TypeVector(kind, sz) => TypeVector(kind addVectorDimension size, sz)
    case kind: TypeFund       => TypeVector(kind, size)
    case _                    => unreachable
  }

}
