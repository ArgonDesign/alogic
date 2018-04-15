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
// Inline param and const values. Clone entities with all parameter bindings.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable

final class InlineConst(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // Map from const symbol to it's value
  private[this] val valueMap = mutable.Map[Symbol, Expr]()

  private[this] object TypeInlineConst extends TreeInTypeTransformer(this)

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Rewrite const declarations with a simplified initializer
    ////////////////////////////////////////////////////////////////////////////

    case decl @ Decl(Sym(symbol: TermSymbol), TypeConst(kind), Some(expr)) => {
      val newKind = TypeConst(kind rewrite TypeInlineConst)
      val init = expr.value match {
        case None => expr
        case Some(value) => {
          val newExpr = ExprInt(newKind.isSigned, newKind.width.value.get.toInt, value)
          TypeAssigner(newExpr withLoc expr.loc)
        }
      }
      valueMap(symbol) = init
      TypeAssigner(decl.copy(kind = newKind, init = Some(init)) withLoc tree.loc)
    }

    ////////////////////////////////////////////////////////////////////////////
    // Inline the value of constants in expressions
    ////////////////////////////////////////////////////////////////////////////

    case ExprRef(Sym(symbol)) => {
      valueMap.getOrElse(symbol, tree)
    }

    ////////////////////////////////////////////////////////////////////////////
    // Inline the value in types
    ////////////////////////////////////////////////////////////////////////////

    case decl @ Decl(Sym(symbol), origKind, _) => {
      val kind = origKind rewrite TypeInlineConst
      if (kind eq origKind) {
        decl
      } else {
        TypeAssigner(decl.copy(kind = kind) withLoc tree.loc)
      }
    }

    case expr @ ExprType(origKind) => {
      val kind = origKind rewrite TypeInlineConst
      if (kind eq origKind) {
        expr
      } else {
        TypeAssigner(expr.copy(kind = kind) withLoc tree.loc)
      }
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    val decls = {
      val it = tree collect {
        case decl @ Decl(_, _: TypeConst, _) => decl
      }
      it.toSet
    }

    tree visit {
      case decl: Decl if decls contains decl => ()
      case node: Tree if node.tpe.isInstanceOf[TypeConst] => {
        cc.ice(node, "const type remains")
      }
      case node @ Sym(symbol) if symbol.denot.kind.isInstanceOf[TypeConst] => {
        cc.ice(node, "const Sym remains")
      }
    }
  }

}
