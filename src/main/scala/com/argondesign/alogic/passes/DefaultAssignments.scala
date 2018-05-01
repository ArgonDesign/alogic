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
// Any symbol that is either:
//  - On the left hand side of an assignment statement
//  - Is an output not driven through a connect
//  - Is an interconnect symbol not driven through a connect
// must be assigned a value on all code paths to avoid latches. In this phase
// we mark all such symbols.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable

final class DefaultAssignments(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  private val needsDefault = mutable.Set[TermSymbol]()

  // Given an expression, return an iterable of symbols that would be assigned
  // should this expression be used on the left hand side of an assignment
  private def writtenSymbols(expr: Expr): Iterator[TermSymbol] = {
    expr match {
      case ExprRef(Sym(symbol: TermSymbol)) => Iterator.single(symbol)
      case ExprCat(parts)                   => parts.toIterator flatMap writtenSymbols
      case ExprIndex(expr, _)               => writtenSymbols(expr)
      case ExprSlice(expr, _, _, _)         => writtenSymbols(expr)
      case _                                => Iterator.empty
    }
  }

  override def enter(tree: Tree): Unit = tree match {
    case StmtAssign(lhs, _) => {
      needsDefault ++= writtenSymbols(lhs)
    }

    case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeOut] => {
      needsDefault += symbol
    }

    case Decl(symbol, _) if symbol.attr.interconnect.isSet => {
      needsDefault += symbol
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {
    // TODO: skip early for verbatim entities
    case entity: Entity if needsDefault.nonEmpty && entity.variant != "verbatim" => {
      // Remove any nets driven through a connect
      for (Connect(_, List(rhs)) <- entity.connects) {
        rhs.visit {
          case ExprRef(Sym(symbol: TermSymbol)) => {
            needsDefault remove symbol
          }
        }
      }

      if (needsDefault.isEmpty) {
        tree
      } else {
        val newFenceStms = for {
          Decl(symbol, _) <- entity.declarations
          if needsDefault contains symbol
        } yield {
          // Initialize items to their default values, otherwise zero
          val init = symbol.attr.default.getOrElse {
            val kind = symbol.denot.kind
            val signed = kind.isSigned
            val width = kind.width.value.get.toInt
            ExprInt(signed, width, 0)
          }
          StmtAssign(ExprRef(Sym(symbol)), init) regularize symbol.loc
        }

        TypeAssigner {
          entity.copy(
            fenceStmts = newFenceStms ::: entity.fenceStmts
          ) withVariant entity.variant withLoc tree.loc
        }
      }
    }

    case _ => tree
  }

}
