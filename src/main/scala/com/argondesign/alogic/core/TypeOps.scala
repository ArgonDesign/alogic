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

import com.argondesign.alogic.Config
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

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
  final def isIdent = this.isInstanceOf[TypeIdent]
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
  final def isType = this.isInstanceOf[TypeType]
  final def isMisc = this eq TypeMisc
  final def isError = this eq TypeError
  final def isPolyFunc = this.isInstanceOf[TypePolyFunc]

  // Is this a primitive numeric type
  final def isNumeric(implicit cc: CompilerContext): Boolean = this match {
    case _: TypeInt         => true
    case _: TypeNum         => true
    case TypeParam(kind)    => kind.isNumeric
    case TypeConst(kind)    => kind.isNumeric
    case TypePipeline(kind) => kind.isNumeric
    case _                  => false
  }

  // Is this a 'packed' type, i.e.: does it have a finite,
  // possibly 0 width bit-vector representation?
  final def isPacked(implicit cc: CompilerContext): Boolean = this match {
    case _: TypeSInt        => true
    case _: TypeUInt        => true
    case _: TypeStruct      => true
    case _: TypeVector      => true
    case TypeVoid           => true
    case self: TypeIn       => self.kind.isPacked
    case self: TypeOut      => self.kind.isPacked
    case self: TypePipeline => self.kind.isPacked
    case self: TypeParam    => self.kind.isPacked
    case self: TypeConst    => self.kind.isPacked
    case _: TypeNum         => Config.treatNumAs32Wide
    case _                  => false
  }

  // Width of this type, assuming it is a packed type
  final def width(implicit cc: CompilerContext): Expr = {
    assert(isPacked)
    this match {
      case self: TypeSInt => self.size
      case self: TypeUInt => self.size
      case self: TypeStruct => {
        if (self.fieldTypes.nonEmpty) {
          val sumExpr = self.fieldTypes map { _.width } reduceLeft { _ + _ }
          sumExpr.simplify
        } else {
          Expr(0) withLoc Loc.synthetic
        }
      }
      case TypeVoid                              => Expr(0) withLoc Loc.synthetic
      case self: TypeVector                      => self.size * self.elementType.width
      case self: TypeIn                          => self.kind.width
      case self: TypeOut                         => self.kind.width
      case self: TypePipeline                    => self.kind.width
      case self: TypeParam                       => self.kind.width
      case self: TypeConst                       => self.kind.width
      case _: TypeNum if Config.treatNumAs32Wide => Expr(32) withLoc Loc.synthetic
      case _                                     => unreachable
    }
  } ensuring { _.hasLoc }

  // Signedness of this type (as far as expressions are concerned), assuming it is a packed type
  final def isSigned(implicit cc: CompilerContext): Boolean = {
    assert(isNum || isPacked, println(this))
    this match {
      case _: TypeSInt     => true
      case TypeNum(signed) => signed
      case _               => false
    }
  }

  // If this is a proxy type, get the underlying type, otherwise get this type
  final lazy val underlying: Type = this match {
    case TypeIn(kind, _)     => kind
    case TypeOut(kind, _, _) => kind
    case TypePipeline(kind)  => kind
    case TypeParam(kind)     => kind
    case TypeConst(kind)     => kind
    case other               => other
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Rewrie with TypeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  final def rewrite(tt: TypeTransformer): Type = tt(this)
}
