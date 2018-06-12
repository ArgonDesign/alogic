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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class AtMax(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@max"

  def returnType(args: List[Expr]) = args partialMatch {
    case args if args.nonEmpty && (args forall { _.isPacked }) => {
      // TODO: return properly computed type (max width)
      TypeUInt(Expr(32))
    }
  }

  def isKnownConst(args: List[Expr]) = args forall { _.isKnownConst }

  def fold(loc: Loc, args: List[Expr]) = {
    (args forall { _.isInstanceOf[ExprNum] }) option {
      args match {
        case Nil => {
          cc.error(loc, "'@max' called with empty parameter list")
          ExprError()
        }
        case List(arg) => arg
        case args => {
          val (s, v) = (args collect { case ExprNum(signed, value) => (signed, value) }).unzip
          ExprNum(s reduceLeft { _ && _ }, v.max)
        }
      }
    }
  }
}
