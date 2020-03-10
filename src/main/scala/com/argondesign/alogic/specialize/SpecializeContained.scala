////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

private[specialize] object SpecializeContained {
  def apply(
      input: Either[Desc, (Decl, Defn)]
  )(
      implicit cc: CompilerContext,
      specializeDesc: SpecializeDesc
  ): Option[(Either[Desc, (Decl, Defn)], collection.Set[Symbol])] = {
    var hadError = false

    val unknowns = mutable.Set[Symbol]()

    val inputSymbol = input.fold({ _.symbol }, { _._1.symbol })

    val mapping = mutable.Map[Symbol, Symbol]()

    def specialize[T <: Tree](
        tree: Tree,
        desc: Desc,
        spliceDecl: Decl => T,
        spliceDefn: Defn => T
    ): Tree = {
      specializeDesc(desc, Map.empty, true, desc.symbol.loc) match {
        case DescSpecializationError            => hadError = true; Stump
        case DescSpecializationUnknown(symbols) => unknowns addAll symbols; tree
        case DescSpecializationComplete(decl, defn, _) =>
          mapping(desc.symbol) = decl.symbol
          Thicket(
            List(
              spliceDecl(decl) withLoc tree.loc,
              spliceDefn(defn) withLoc tree.loc
            ))
        // We are specializing the Desc node itself, so this cannot happen
        case _: DescSpecializationPending => unreachable
      }
    }

    val transform: StatefulTreeTransformer = new StatefulTreeTransformer {
      override val typed: Boolean = false

      override def enter(tree: Tree): Option[Tree] = tree match {
        // Skip Decl nodes. These must be fully specialized by now
        case _: Decl => Some(tree)
        // Skip Defn nodes if it's not the current symbol (same reason as above).
        case defn: Defn if defn.symbol != inputSymbol => Some(tree)
        // Skip DescChoice nodes
        case EntDesc(_: DescChoice)  => Some(tree)
        case RecDesc(_: DescChoice)  => Some(tree)
        case StmtDesc(_: DescChoice) => Some(tree)
        // Skip DescParam nodes
        case EntDesc(_: DescParam) => Some(tree)
        case RecDesc(_: DescParam) => Some(tree)
        // Skip parametrized Desc nodes
        case EntDesc(desc) if desc.isParametrized  => Some(tree)
        case RecDesc(desc) if desc.isParametrized  => Some(tree)
        case StmtDesc(desc) if desc.isParametrized => Some(tree)
        // Specialize Desc nodes
        case EntDesc(desc)  => Some(specialize(tree, desc, EntDecl, EntDefn))
        case RecDesc(desc)  => Some(specialize(tree, desc, RecDecl, RecDefn))
        case StmtDesc(desc) => Some(specialize(tree, desc, StmtDecl, StmtDefn))
        // Specialize encountered expressions
        case expr: Expr =>
          Some {
            SpecializeExpr(expr) match {
              case ExprSpecializationError            => hadError = true; tree
              case ExprSpecializationUnknown(symbols) => unknowns addAll symbols; tree
              case ExprSpecializationComplete(result) => result
            }
          }
        // Specialize Gen node headers, but not bodies
        case gen @ GenIf(cond, _, _) =>
          Some {
            gen.copy(cond = walk(cond).asInstanceOf[Expr]) withLoc tree.loc
          }
        case gen @ GenFor(inits, cond, steps, _) =>
          Some {
            gen.copy(
              inits = walk(inits).asInstanceOf[List[Stmt]],
              cond = walk(cond).asInstanceOf[Expr],
              steps = walk(steps).asInstanceOf[List[Stmt]]
            ) withLoc tree.loc
          }
        case gen @ GenRange(inits, _, end, _) =>
          Some {
            gen.copy(
              inits = walk(inits).asInstanceOf[List[Stmt]],
              end = walk(end).asInstanceOf[Expr]
            ) withLoc tree.loc
          }
        //
        case _ => None
      }
    }

    val result = input.fold({ desc =>
      Left(desc rewrite transform)
    }, {
      case (decl, defn) => Right((decl, defn rewrite transform))
    })

    if (hadError) None else Some((Replace(result, mapping), unknowns))
  }
}
