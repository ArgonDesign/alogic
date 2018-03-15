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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.transform.ChaseTypeRefs
import com.argondesign.alogic.util.unreachable

import Types._

trait TypeOps { this: Type =>

  // Is this a primitive numeric type
  final lazy val isNumeric: Boolean = this match {
    case _: TypeInt         => true
    case _: TypeNum         => true
    case TypeParam(kind)    => kind.isNumeric
    case TypeConst(kind)    => kind.isNumeric
    case TypePipeline(kind) => kind.isNumeric
    case _                  => false
  }

  // Is this a 'packed' type, i.e.: does it have a finite bit-vector representation?
  final lazy val isPacked: Boolean = this match {
    case _: TypeSInt        => true
    case _: TypeUInt        => true
    case _: TypeStruct      => true
    case _: TypeVector      => true
    case self: TypeIn       => self.kind.isPacked
    case self: TypeOut      => self.kind.isPacked
    case self: TypePipeline => self.kind.isPacked
    case self: TypeParam    => self.kind.isPacked
    case self: TypeConst    => self.kind.isPacked
    case _                  => false
  }

  // Width of this type, assuming it is a packed type
  final lazy val width: Expr = {
    assert(isPacked)
    this match {
      case self: TypeSInt => self.size
      case self: TypeUInt => self.size
      case self: TypeStruct => {
        if (self.fieldTypes.nonEmpty) {
          self.fieldTypes map { _.width } reduceLeft { _ + _ }
        } else {
          Expr(0) withLoc Loc.synthetic
        }
      }
      case self: TypeVector   => self.size * self.elementType.width
      case self: TypeIn       => self.kind.width
      case self: TypeOut      => self.kind.width
      case self: TypePipeline => self.kind.width
      case self: TypeParam    => self.kind.width
      case self: TypeConst    => self.kind.width
      case _                  => unreachable
    }
  } ensuring { _.hasLoc }

  // Signedness of this type (as far as expressions are concerned), assuming it is a packed type
  final lazy val isSigned: Boolean = {
    assert(isPacked)
    this match {
      case _: TypeSInt     => true
      case TypeNum(signed) => signed
      case _               => false
    }
  }

  // Follow TypeRef instances to the underlying types
  final def chase(implicit cc: CompilerContext): Type = this rewrite (new ChaseTypeRefs)

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
