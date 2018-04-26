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
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeCombFunc
import com.argondesign.alogic.core.Types.TypePolyFunc
import com.argondesign.alogic.util.BooleanOps
import com.argondesign.alogic.util.PartialMatch

import scala.collection.concurrent.TrieMap

private[builtins] abstract class BuiltinPolyFunc(implicit cc: CompilerContext)
    extends BooleanOps
    with PartialMatch {

  //////////////////////////////////////////////////////////////////////////////
  // Public interface
  //////////////////////////////////////////////////////////////////////////////

  final def symbol: TermSymbol = {
    if (_symbol == null) {
      _symbol = cc.newTermSymbol(ident, TypePolyFunc(resolver))
    }
    _symbol
  }

  // Check if symbol is referring to this BuiltinPolyFunc, or is an
  // overload of this BuiltinPolyFunc
  final def contains(symbol: Symbol): Boolean = {
    (_symbol == symbol) || (overloads.values.iterator.flatten contains symbol)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Abstract methods
  //////////////////////////////////////////////////////////////////////////////

  // Name of builtin function
  protected[this] val name: String

  // Type of return value for the given arguments, or None if these arguments are invalid
  protected[this] def returnType(args: List[Expr]): Option[Type]

  // Is this a known compile time constant?
  private[builtins] def isKnownConst(args: List[Expr]): Boolean

  // Fold calls to this function
  private[builtins] def fold(loc: Loc, args: List[Expr]): Option[Expr]

  //////////////////////////////////////////////////////////////////////////////
  // Implementation
  //////////////////////////////////////////////////////////////////////////////

  private[this] final var _symbol: TermSymbol = _

  // Synthetic location of this builtin
  protected[this] final lazy val loc = Loc(Source(s"builtin ${name}", ""), 0, 0, 0)

  // Synthetic identifier
  private[this] final lazy val ident = Ident(name) withLoc loc

  // Collection of overloaded symbols (if any) for given arguments
  // TODO: This map should be in cc to avoid a space leak
  private[this] final val overloads = TrieMap[List[Expr], Option[TermSymbol]]()

  // The resolver for TypePolyFunc
  private[this] final def resolver(args: List[Expr]) = {
    overloads.getOrElseUpdate(args, returnType(args).map { kind =>
      cc.newTermSymbol(ident, TypeCombFunc(args map { _.tpe }, kind))
    })
  }
}
