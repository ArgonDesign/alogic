////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Base for builtin polymorphic functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeCombFunc
import com.argondesign.alogic.core.Types.TypeFund
import com.argondesign.alogic.core.Types.TypePolyFunc
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.PartialMatch
import com.argondesign.alogic.util.unreachable

import scala.collection.concurrent.TrieMap
import scala.util.ChainingSyntax

abstract class BuiltinPolyFunc(implicit cc: CompilerContext)
    extends PartialMatch
    with ChainingSyntax {

  //////////////////////////////////////////////////////////////////////////////
  // Public interface
  //////////////////////////////////////////////////////////////////////////////

  lazy val symbol: Symbol = cc.newSymbol(name, loc) tap { s =>
    s.kind = TypePolyFunc(s, resolver)
    s.attr.builtin set this
  }

  //////////////////////////////////////////////////////////////////////////////
  // Function specific methods
  //////////////////////////////////////////////////////////////////////////////

  // Name of builtin function
  protected[this] val name: String

  // Type of return value for given arguments, or None if these arguments are invalid
  // TODO: Should return frontend.Result[TypeFund] and messages properly
  protected[this] def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund]

  // Is this a pure function?
  protected[builtins] val isPure: Boolean

  // Fold calls to this function
  protected[this] def simplify(loc: Loc, args: List[Expr]): Option[Expr]

  //////////////////////////////////////////////////////////////////////////////
  // Implementation
  //////////////////////////////////////////////////////////////////////////////

  // Fold calls to this function
  private[builtins] def fold(loc: Loc, args: List[Arg]): Option[Expr] = simplify(loc, pargs(args))

  // Synthetic location of this builtin
  final protected[this] lazy val loc = Loc(s"builtin $name", 0, Source("", ""), 0, 0, 0)

  // Collection of overloaded symbols (if any) for given arguments
  // TODO: This map should be in cc to avoid a space leak
  final private[this] val overloads = TrieMap[(Type, List[Type]), Symbol]()

  // The resolver for TypePolyFunc
  final private[this] def resolver(args: List[Arg], feOpt: Option[Frontend]): Option[Symbol] = {
    val pas = pargs(args)
    returnType(pas, feOpt) map { retType =>
      val argTypes = pas map { _.tpe }
      overloads.getOrElseUpdate(
        (retType, argTypes),
        cc.newSymbol(name, loc) tap { s =>
          s.kind = TypeCombFunc(symbol, retType, argTypes)
          s.attr.builtin set this
        }
      )
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Helpers
  //////////////////////////////////////////////////////////////////////////////

  private[builtins] def pargs(args: List[Arg]): List[Expr] = args flatMap {
    case ArgP(expr) => Some(expr)
    case arg: ArgN  => cc.error(arg, s"Cannot pass named arguments to builtin '$name'"); None
    case _: ArgD    => unreachable
  }

}
