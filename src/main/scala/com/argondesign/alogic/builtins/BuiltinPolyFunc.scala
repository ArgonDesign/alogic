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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeCombFunc
import com.argondesign.alogic.core.Types.TypeFund
import com.argondesign.alogic.core.Types.TypePolyFunc
import com.argondesign.alogic.util.BooleanOps
import com.argondesign.alogic.util.PartialMatch

import scala.collection.concurrent.TrieMap
import scala.util.ChainingSyntax

private[builtins] abstract class BuiltinPolyFunc(
    val isValidConnLhs: Boolean
)(
    implicit cc: CompilerContext
) extends BooleanOps
    with PartialMatch
    with ChainingSyntax {

  //////////////////////////////////////////////////////////////////////////////
  // Public interface
  //////////////////////////////////////////////////////////////////////////////

  final def symbol: Symbol = {
    if (_symbol == null) {
      _symbol = cc.newSymbol(name, loc) tap { s =>
        s.kind = TypePolyFunc(s, resolver)
      }
    }
    _symbol
  }

  // Check if symbol is referring to this BuiltinPolyFunc, or is an
  // overload of this BuiltinPolyFunc
  final def contains(symbol: Symbol): Boolean = {
    (_symbol == symbol) || (overloads.values.iterator.flatten contains symbol)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Function specific methods
  //////////////////////////////////////////////////////////////////////////////

  // Name of builtin function
  protected[this] val name: String

  // Type of return value for given arguments, or None if these arguments are invalid
  protected[this] def returnType(args: List[Expr]): Option[TypeFund]

  // Is this a known compile time constant?
  protected[this] def isKnown(args: List[Expr]): Boolean

  // Can this call be exist on the lhs of a Connect?
  protected[this] def isValidConnLhs(args: List[Expr]): Boolean = false

  // Fold calls to this function
  protected[this] def simplify(loc: Loc, args: List[Expr]): Option[Expr]

  //////////////////////////////////////////////////////////////////////////////
  // Implementation
  //////////////////////////////////////////////////////////////////////////////

  // Is this a known compile time constant?
  private[builtins] def isKnownConst(args: List[Arg]): Boolean = isKnown(pargs(args))

  // Fold calls to this function
  private[builtins] def fold(loc: Loc, args: List[Arg]): Option[Expr] = simplify(loc, pargs(args))

  // Is this valid on the left hand side of a connect?
  private[builtins] def isValidConnectLhs(args: List[Arg]): Boolean =
    (pargs(args) forall { expr =>
      expr.isKnownConst || expr.isValidConnectLhs
    }) && (isValidConnLhs || isKnownConst(args))

  private[this] final var _symbol: Symbol = _

  // Synthetic location of this builtin
  protected[this] final lazy val loc = Loc(Source(s"builtin $name", ""), 0, 0, 0)

  // Collection of overloaded symbols (if any) for given arguments
  // TODO: This map should be in cc to avoid a space leak
  private[this] final val overloads = TrieMap[List[Arg], Option[Symbol]]()

  // The resolver for TypePolyFunc
  private[this] final def resolver(args: List[Arg]): Option[Symbol] = {
    overloads.getOrElseUpdate(
      args, {
        val pas = pargs(args)
        returnType(pas) map { retType =>
          val argTypes = pas map { _.tpe }
          cc.newSymbol(name, loc) tap { _.kind = TypeCombFunc(symbol, retType, argTypes) }
        }
      }
    )
  }

  //////////////////////////////////////////////////////////////////////////////
  // Helpers
  //////////////////////////////////////////////////////////////////////////////

  private[builtins] def pargs(args: List[Arg]): List[Expr] = args flatMap {
    case ArgP(expr) => Some(expr)
    case arg: ArgN  => cc.error(arg, s"Cannot pass named arguments to builtin '$name'"); None
  }

}
