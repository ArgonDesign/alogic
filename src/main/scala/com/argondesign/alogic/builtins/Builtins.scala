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
// Builtin symbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

trait Builtins { this: CompilerContext =>

  private implicit val implicitThis = this

  // Register this builtin in the global scope
  private[this] def register(builtin: BuiltinPolyFunc): builtin.type = {
    addGlobalSymbol(builtin.symbol)
    builtin
  }

  val builtins = Set[BuiltinPolyFunc](
    register(new AtBits),
    register(new AtEx),
    register(new AtMax),
    register(new AtMsb),
    register(new AtRandbit),
    register(new AtSx),
    register(new AtZx),
    register(new DollarClog2),
    register(new DollarSigned),
    register(new DollarUnigned)
  )

  // Fold call to builtin function
  def foldBuiltinCall(call: ExprCall): Expr = {
    val ExprRef(Sym(symbol)) = call.expr
    val Some(builtin) = builtins find { _ contains symbol }
    builtin.fold(call.loc, call.args) map { _ assignLocs call.loc } getOrElse call
  }

  // Fold call to builtin function
  def isKnownConstBuiltinCall(call: ExprCall): Boolean = {
    val ExprRef(Sym(symbol)) = call.expr
    val Some(builtin) = builtins find { _ contains symbol }
    builtin.isKnownConst(call.args)
  }
}
