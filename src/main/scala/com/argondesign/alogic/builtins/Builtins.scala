////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Lookup/symbol table for builtin functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.frontend.SymbolTable

trait Builtins { this: CompilerContext =>

  private val builtins: Map[Symbol, Builtin] = Iterator(
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
  ).map(b => new Symbol(b.name) -> b).toMap

  def getBuiltin(symbol: Symbol): Option[Builtin] = builtins.get(symbol)

  // Symbol table holding all builtin symbols
  val builtinSymbolTable: SymbolTable = builtins.foldLeft(SymbolTable.empty) {
    case (st, (symbol, _)) => st + symbol
  }

}
