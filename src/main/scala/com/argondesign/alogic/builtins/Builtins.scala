////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin symbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.frontend.SymbolTable
import com.argondesign.alogic.util.unreachable

trait Builtins { this: CompilerContext =>

  implicit private val implicitThis: CompilerContext = this

  // Symbol table holding all builtin symbols
  val builtins: SymbolTable = Iterable[BuiltinPolyFunc](
    new AtBits,
    new AtEx,
    new AtMax,
    new AtMsb,
    new AtRandbit,
    new AtSx,
    new AtZx,
    new DollarClog2,
    new DollarDisplay,
    new DollarFinish,
    new DollarSigned,
    new DollarUnsigned
  ).foldLeft(SymbolTable.empty) { case (st, builtin) => st + builtin.symbol }

  // Fold call to builtin function
  def foldBuiltinCall(call: ExprCall): Expr = call.expr match {
    case ExprSym(symbol) =>
      val builtin = symbol.attr.builtin.value
      builtin.fold(call.loc, call.args) map {
        _ regularize call.loc
      } getOrElse call
    case _ => unreachable
  }

  // Is this a pure call
  def isPureBuiltinCall(call: ExprCall): Boolean = call.expr match {
    case ExprSym(symbol) =>
      symbol.attr.builtin.value.isPure && (call.args forall { _.expr.isPure })
    case _ => unreachable
  }

}
