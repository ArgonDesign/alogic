////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Inline any variables that we know the value of
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class InlineKnownVars(
    combOnly: Boolean
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private[this] var stmtBindings: Map[Int, Bindings] = _
  private[this] var endOfCycleBindings: Bindings = _

  private[this] val bindings = mutable.Stack[Bindings]()

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      assert(defn.combProcesses.lengthIs <= 1)
      defn.combProcesses.headOption foreach {
        case EntCombProcess(body) =>
          StaticEvaluation(StmtBlock(body), Nil) match {
            case None =>
            case Some(evaluation) =>
              stmtBindings = evaluation._1
              endOfCycleBindings = if (combOnly) Bindings.empty else evaluation._2
              bindings.push(endOfCycleBindings)
          }
      }
    case _ =>
  }

  override def skip(tree: Tree): Boolean = bindings.isEmpty

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Don't fold constants on the lhs of assignment TODO: fold the read ones...
    case stmt @ StmtAssign(_, rhs) =>
      Some {
        bindings.push(stmtBindings.getOrElse(stmt.id, Bindings.empty))
        TypeAssigner(stmt.copy(rhs = walk(rhs).asInstanceOf[Expr]) withLoc tree.loc) tap { _ =>
          bindings.pop()
        }
      }

    case stmt @ StmtOutcall(_, func, inputs) =>
      Some {
        bindings.push(stmtBindings.getOrElse(stmt.id, Bindings.empty))
        TypeAssigner(
          stmt.copy(
            func = walk(func).asInstanceOf[Expr],
            inputs = walk(inputs).asInstanceOf[List[Expr]]
          ) withLoc tree.loc
        ) tap { _ =>
          bindings.pop()
        }
      }

    case stmt: Stmt =>
      bindings.push(stmtBindings.getOrElse(stmt.id, endOfCycleBindings))
      None

    // Only substitute in index/slice target if the indices are known constants,
    // in which case fold them as well. This is to avoid creating non-constant
    // indices into non-symbols
    case e @ ExprIndex(expr, index) =>
      Some {
        walk(index).asInstanceOf[Expr].simplify match {
          case known: ExprInt =>
            val newExpr = walk(expr).asInstanceOf[Expr]
            TypeAssigner(ExprIndex(newExpr, known) withLoc tree.loc).simplify
          case other => TypeAssigner(e.copy(index = other) withLoc tree.loc)
        }
      }

    case e @ ExprSlice(expr, lIdx, _, _) =>
      Some {
        walk(lIdx).asInstanceOf[Expr].simplify match {
          case known: ExprInt =>
            val newExpr = walk(expr).asInstanceOf[Expr]
            // Note: rIdx is always constant
            TypeAssigner(e.copy(expr = newExpr, lIdx = known) withLoc tree.loc).simplify
          case other => TypeAssigner(e.copy(lIdx = other) withLoc tree.loc)
        }
      }

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Substitute known constants
    case ExprSym(symbol) =>
      bindings.top.get(symbol) map { _.simplify } match {
        case Some(expr: ExprInt) => expr
//        case Some(expr: ExprNum) => expr
        case _ => tree
      }

    case _: Stmt =>
      bindings.pop()
      tree

    case _ => tree
  }

}

object InlineKnownVars {

  def apply(combOnly: Boolean): EntityTransformerPass =
    new EntityTransformerPass(declFirst = false) {
      val name = "inline-known-vars"

      override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
        super.skip(decl, defn) || defn.asInstanceOf[DefnEntity].combProcesses.isEmpty

      def create(symbol: Symbol)(implicit cc: CompilerContext) = new InlineKnownVars(combOnly)
    }

}
