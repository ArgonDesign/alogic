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
import com.argondesign.alogic.core.Types._

object AtBits extends BuiltinPolyFunc {

  protected def name = "@bits"

  // TODO: return TypeNum
  protected def retType(args: List[Expr]): Type = TypeUInt(Expr(32) withLoc loc)

  protected def validArgs(args: List[Expr]) = {
    args.lengthCompare(1) == 0 && {
      args.head.tpe match {
        case TypeType(kind) if kind.isPacked => true
        case kind if kind.isPacked           => true
        case _                               => false
      }
    }
  }

  // TODO: implement fold
}
