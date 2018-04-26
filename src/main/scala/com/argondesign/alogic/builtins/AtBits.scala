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

private[builtins] class AtBits(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@bits"

  def returnType(args: List[Expr]) = args match {
    case List(arg) => {
      arg.tpe match {
        case _: TypeNum                      => None
        case TypeType(kind) if kind.isPacked => Some(TypeNum(false))
        case kind if kind.isPacked           => Some(TypeNum(false))
        case _                               => None
      }
    }
    case _ => None
  }

  def isKnownConst(args: List[Expr]) = true

  def fold(loc: Loc, args: List[Expr]) = {
    args.head.tpeOpt map {
      case TypeType(kind) => kind.width
      case kind           => kind.width
    }
  }
}
