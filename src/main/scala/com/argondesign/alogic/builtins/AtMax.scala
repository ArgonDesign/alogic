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
// Builtin '@max'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

object AtMax extends BuiltinPolyFunc {

  protected def name = "@max"

  // TODO: return properly computed type
  protected def retType(args: List[Expr], cc: CompilerContext): Type = {
    TypeUInt(Expr(32) withLoc loc)
  }

  protected def validArgs(args: List[Expr], cc: CompilerContext) = {
    args forall { _.tpe.isPacked }
  }

  private[builtins] override def fold(call: ExprCall, cc: CompilerContext): Expr = {
    val args = call.args
    if (args exists { !_.isInstanceOf[ExprNum] }) {
      call
    } else {
      if (args.length >= 2) {
        val (s, v) = (args collect { case ExprNum(signed, value) => (signed, value) }).unzip
        ExprNum(s reduceLeft { _ && _ }, v.max) withLoc call.loc
      } else if (args.length == 1) {
        args.head
      } else {
        cc.error(call, "Reslt of '@max()' is not well defined")
        ExprError() withLoc call.loc
      }
    }
  }

}
