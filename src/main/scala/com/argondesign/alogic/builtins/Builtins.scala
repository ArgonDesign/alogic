////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin symbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.frontend.SymbolTable
import com.argondesign.alogic.util.unreachable

object Builtins {

  // Symbol table holding all builtin symbols
  lazy val symbolTable: SymbolTable = Iterable[BuiltinPolyFunc](
    AtBits,
    AtEx,
    AtMax,
    AtMsb,
    AtUnknownI,
    AtUnknownU,
    AtSx,
    AtZx,
    DollarClog2,
    DollarDisplay,
    DollarFinish,
    DollarSigned,
    DollarUnsigned
  ).foldLeft(SymbolTable.empty) { case (st, builtin) => st + builtin.symbol }

  def makeCall(name: String, loc: Loc, args: List[Expr]): ExprCall = {
    val polySymbol = symbolTable.get(name) match {
      case SymbolTable.Defined(symbol) => symbol
      case _                           => throw Ice(s"Attempting to construct unknown builtin '$name'")
    }
    assert(polySymbol.isBuiltin)
    assert(args exists { _.hasTpe })
    val argps = args map { a => ArgP(a).regularize(a.loc) }
    val symbol = polySymbol.kind.asPolyFunc.resolve(argps, None).get
    val call = ExprSym(symbol).call(argps)
    call.regularize(loc)
  }

  // Fold call to builtin function
  def foldCall(call: ExprCall): Expr = call.expr match {
    case ExprSym(symbol) =>
      val builtin = symbol.attr.builtin.value
      builtin.fold(call.loc, call.args) map {
        _ regularize call.loc
      } getOrElse call
    case _ => unreachable
  }

  // Is this a pure call
  def isPureCall(call: ExprCall): Boolean = call.expr match {
    case ExprSym(symbol) => symbol.attr.builtin.value.isPure && (call.args forall { _.expr.isPure })
    case _               => unreachable
  }

}
