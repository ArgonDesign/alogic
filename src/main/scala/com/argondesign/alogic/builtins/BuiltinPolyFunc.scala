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
// Base for builtin polymorphic functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.ast.Trees.ExprCall
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeCombFunc
import com.argondesign.alogic.core.Types.TypePolyFunc

import scala.collection.mutable

private[builtins] trait BuiltinPolyFunc {

  //////////////////////////////////////////////////////////////////////////////
  // Public interface
  //////////////////////////////////////////////////////////////////////////////

  final def symbol(implicit cc: CompilerContext): TermSymbol = {
    if (_symbol == null) {
      val kind = TypePolyFunc(resolver)
      _symbol = cc.newTermSymbol(ident, kind)
    }
    _symbol
  }

  // Check if symbol is referring to this BuiltinPolyFunc, or is an
  // overload of this BuiltinPolyFunc
  final def contains(symbol: Symbol): Boolean = synchronized {
    (_symbol == symbol) || (overloads.values.iterator contains symbol)
  }

  // Fold calls to this function
  private[builtins] def fold(call: ExprCall)(implicit cc: CompilerContext): Expr = {
    call
  }

  private[builtins] def isKnownConst(call: ExprCall)(implicit cc: CompilerContext): Boolean = {
    false
  }

  //////////////////////////////////////////////////////////////////////////////
  // Abstract methods
  //////////////////////////////////////////////////////////////////////////////

  // Name of builtin function
  protected[this] def name: String

  // Type of return value for the given arguments
  protected[this] def retType(args: List[Expr])(implicit cc: CompilerContext): Type

  // Predicate that checks whether this BuiltinPolyFunc can be applied to these
  // arguments. Assumes args _.hasTpe
  protected[this] def validArgs(args: List[Expr])(implicit cc: CompilerContext): Boolean

  //////////////////////////////////////////////////////////////////////////////
  // Implementation
  //////////////////////////////////////////////////////////////////////////////

  private[this] final var _symbol: TermSymbol = _

  // Synthetic location of this builtin
  protected[this] final val loc = Loc(Source(s"builtin ${name}", ""), 0, 0, 0)

  // Synthetic identifier
  private[builtins] final val ident = Ident(name) withLoc loc

  // Collection of overloaded symbols for given arguments
  private[this] final val overloads = mutable.Map[List[Expr], TermSymbol]()

  // Return the overloaded symbol for these arguments
  private[this] final def getOverload(args: List[Expr])(
      implicit cc: CompilerContext): TermSymbol = {
    synchronized {
      lazy val newSymbol = {
        val argTypes = args map { _.tpe }
        val kind = TypeCombFunc(argTypes, retType(args))
        cc.newTermSymbol(ident, kind)
      }
      overloads.getOrElseUpdate(args, newSymbol)
    }
  }

  // The resolver for TypePolyFunc
  private[this] final def resolver(args: List[Expr])(cc: CompilerContext) = {
    if (validArgs(args)(cc)) Some(getOverload(args)(cc)) else None
  }

}
