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

  // Register this builtin in the global scope
  private[this] def register(builtin: BuiltinPolyFunc): builtin.type = {
    addGlobalSymbol(builtin.symbol(this))
    builtin
  }

  val builtins = Set[BuiltinPolyFunc](
    register(AtBits),
    register(AtMax),
    register(AtZx)
  )

  // Fold call to builtin function
  def foldBuiltinCall(call: ExprCall): Expr = {
    val ExprRef(Sym(symbol)) = call.expr
    val Some(builtin) = builtins find { _ contains symbol }
    builtin.fold(call, this)
  }

  // Fold call to builtin function
  def isKnownConstBuiltinCall(call: ExprCall): Boolean = {
    val ExprRef(Sym(symbol)) = call.expr
    val Some(builtin) = builtins find { _ contains symbol }
    builtin.isKnownConst(call, this)
  }
}
