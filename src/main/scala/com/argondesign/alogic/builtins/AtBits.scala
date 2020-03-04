////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Builtin '@bits'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class AtBits(implicit cc: CompilerContext)
    extends BuiltinPolyFunc(isValidConnLhs = true) {

  val name = "@bits"

  def returnType(args: List[Expr]): Option[TypeFund] = {
    args map { _.tpe } match {
      case List(TypeType(kind)) if kind.isPacked => Some(TypeNum(false))
      case List(TypeNone(kind)) if kind.isPacked => Some(TypeNum(false))
      case List(kind) if kind.isPacked           => Some(TypeNum(false))
      case _                                     => None
    }
  }

  def isKnown(args: List[Expr]) = true

  def simplify(loc: Loc, args: List[Expr]) = {
    args.head.tpeOpt map {
      case TypeType(kind) => Expr(kind.width)
      case TypeNone(kind) => Expr(kind.width)
      case kind           => Expr(kind.width)
    }
  }
}
