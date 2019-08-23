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
import com.argondesign.alogic.transform.DereferenceTypeRefs
import com.argondesign.alogic.util.unreachable

import scala.language.postfixOps

trait TypeOps extends TypePrintOps { this: Type =>

  // Primitive but common type tests
  final def isCombStmt = this eq TypeCombStmt
  final def isCtrlStmt = this eq TypeCtrlStmt
  final def isState = this eq TypeState
  final def isInt = this.isInstanceOf[TypeInt]
  final def isSInt = this.isInstanceOf[TypeSInt]
  final def isUInt = this.isInstanceOf[TypeUInt]
  final def isNum = this.isInstanceOf[TypeNum]
  final def isVector = this.isInstanceOf[TypeVector]
  final def isArray = this.isInstanceOf[TypeArray]
  final def isStack = this.isInstanceOf[TypeStack]
  final def isSram = this.isInstanceOf[TypeSram]
  final def isStruct = this.isInstanceOf[TypeStruct]
  final def isVoid = this eq TypeVoid
  final def isRef = this.isInstanceOf[TypeRef]
  final def isCombFunc = this.isInstanceOf[TypeCombFunc]
  final def isCtrlFunc = this.isInstanceOf[TypeCtrlFunc]
  final def isEntity = this.isInstanceOf[TypeEntity]
  final def isInstance = this.isInstanceOf[TypeInstance]
  final def isStr = this eq TypeStr
  final def isIn = this.isInstanceOf[TypeIn]
  final def isOut = this.isInstanceOf[TypeOut]
  final def isPipeline = this.isInstanceOf[TypePipeline]
  final def isParam = this.isInstanceOf[TypeParam]
  final def isConst = this.isInstanceOf[TypeConst]
  final def isGen = this.isInstanceOf[TypeGen]
  final def isType = this.isInstanceOf[TypeType]
  final def isMisc = this eq TypeMisc
  final def isError = this eq TypeError
  final def isPolyFunc = this.isInstanceOf[TypePolyFunc]
  final def isChoice = this.isInstanceOf[TypeChoice]

  // ... equivalent casts
  final def asCombStmt = TypeCombStmt
  final def asCtrlStmt = TypeCtrlStmt
  final def asState = TypeState
  final def asInt = this.asInstanceOf[TypeInt]
  final def asSInt = this.asInstanceOf[TypeSInt]
  final def asUInt = this.asInstanceOf[TypeUInt]
  final def asNum = this.asInstanceOf[TypeNum]
  final def asVector = this.asInstanceOf[TypeVector]
  final def asArray = this.asInstanceOf[TypeArray]
  final def asStack = this.asInstanceOf[TypeStack]
  final def asSram = this.asInstanceOf[TypeSram]
  final def asStruct = this.asInstanceOf[TypeStruct]
  final def asVoid = TypeVoid
  final def asRef = this.asInstanceOf[TypeRef]
  final def asCombFunc = this.asInstanceOf[TypeCombFunc]
  final def asCtrlFunc = this.asInstanceOf[TypeCtrlFunc]
  final def asEntity = this.asInstanceOf[TypeEntity]
  final def asInstance = this.asInstanceOf[TypeInstance]
  final def asStr = TypeStr
  final def asIn = this.asInstanceOf[TypeIn]
  final def asOut = this.asInstanceOf[TypeOut]
  final def asPipeline = this.asInstanceOf[TypePipeline]
  final def asParam = this.asInstanceOf[TypeParam]
  final def asConst = this.asInstanceOf[TypeConst]
  final def asGen = this.asInstanceOf[TypeGen]
  final def asType = this.asInstanceOf[TypeType]
  final def asMisc = TypeMisc
  final def asError = TypeError
  final def asPolyFunc = this.asInstanceOf[TypePolyFunc]
  final def asChoice = this.asInstanceOf[TypeChoice]

  // Is this a primitive numeric type
  final def isNumeric(implicit cc: CompilerContext): Boolean = this.underlying match {
    case _: TypeInt => true
    case _: TypeNum => true
    case _          => false
  }

  // Is this a 'packed' type, i.e.: does it have a finite,
  // possibly 0 width bit-vector representation?
  final def isPacked(implicit cc: CompilerContext): Boolean = this.underlying match {
    case _: TypeSInt   => true
    case _: TypeUInt   => true
    case _: TypeStruct => true
    case _: TypeVector => true
    case TypeVoid      => true
    case _             => false
  }

  // Signedness of this type (as far as expressions are concerned), assuming it is a packed type
  final def isSigned(implicit cc: CompilerContext): Boolean = {
    assert(underlying.isNum || isPacked, this)
    this.underlying match {
      case _: TypeSInt     => true
      case TypeNum(signed) => signed
      case _               => false
    }
  }

  // Width of this type, assuming it is a packed type
  final def width(implicit cc: CompilerContext): Int = {
    assert(isPacked)
    try {
      this.underlying match {
        case self: TypeSInt => self.size.value.get.toInt
        case self: TypeUInt => self.size.value.get.toInt
        case self: TypeStruct =>
          self.fieldTypes map {
            _.width
          } sum
        case TypeVoid         => 0
        case self: TypeVector => self.size.value.get.toInt * self.elementType.width
        case _                => unreachable
      }
    } catch {
      case t: Throwable => {
        cc.error(s"Cannot compute width of type ${this.toSource}", this.toString)
        this.underlying match {
          case self: TypeSInt =>
            cc.error(self.size.loc, "Size expression:", self.size.toSource, self.size.toString)
          case self: TypeUInt =>
            cc.error(self.size.loc, "Size expression:", self.size.toSource, self.size.toString)
          case _ => cc.error("")
        }
        throw t
      }
    }
  }

  final def shapeIter(implicit cc: CompilerContext): Iterator[Int] = this.underlying match {
    case TypeArray(elemKind, shape)  => Iterator.single(shape.value.get.toInt) ++ elemKind.shapeIter
    case TypeVector(elemKind, shape) => Iterator.single(shape.value.get.toInt) ++ elemKind.shapeIter
    case kind if kind.isPacked       => Iterator.single(kind.width)
    case _                           => Iterator.empty
  }

  // Chase TypeRef nodes
  final def deref(implicit cc: CompilerContext): Type = this rewrite new DereferenceTypeRefs

  // If this is a proxy type, get the underlying type, otherwise get this type
  final def underlying(implicit cc: CompilerContext): Type = this.deref match {
    case TypeIn(kind, _)     => kind
    case TypeOut(kind, _, _) => kind
    case TypePipeline(kind)  => kind
    case TypeParam(kind)     => kind
    case TypeConst(kind)     => kind
    case TypeGen(kind)       => kind
    case other               => other
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Rewrite with TypeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  final def rewrite(tt: TypeTransformer): Type = tt(this)
}
