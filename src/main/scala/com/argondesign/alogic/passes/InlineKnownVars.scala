////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Inline any variables that we know the value of
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.util.BigIntOps._

import scala.annotation.tailrec
import scala.collection.mutable

final class InlineKnownVars(
    combOnly: Boolean = true
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private var stmtBindings: Map[Int, Bindings] = _
  private var endOfCycleBindings: Bindings = _
  private var temporariesToDrop: Set[Symbol] = _

  private val bindings = mutable.Stack[Bindings]()

  def computeEvaluation(stmt: Stmt): Unit = StaticEvaluation(stmt, Bindings.empty) match {
    case None =>
    case Some(evaluation) =>
      stmtBindings = evaluation._1
      endOfCycleBindings = if (combOnly) Bindings.empty else evaluation._2
      bindings.push(endOfCycleBindings)
      val readCount = evaluation._4
      temporariesToDrop = Set from { // Drop temporaries read once
        readCount.iterator.collect { case (symbol, 1) if symbol.attr.tmp.isSet => symbol }
      }
  }

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      assert(defn.combProcesses.lengthIs <= 1)
      defn.combProcesses.headOption foreach { proc => computeEvaluation(StmtBlock(proc.stmts)) }
    case defn: DefnState =>
      assert(combOnly)
      computeEvaluation(StmtBlock(defn.body))
    case stmt: Stmt =>
      assert(combOnly)
      computeEvaluation(stmt)
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

    // Selects on structs are removed by SplitStruct but InlineKnownVars might
    // be called prior, e.g. from InlineMethods, so handle them if we can.
    case e @ ExprSel(expr, sel) if expr.tpe.underlying.isRecord =>
      Some {
        walk(expr).asInstanceOf[Expr].simplify match {
          case known: ExprInt =>
            val kind = expr.tpe.underlying.asRecord
            val dataMembers = kind.dataMembers.reverse // Reverse for big-endian packing
            val fieldIndex = dataMembers.indexWhere(_.name == sel)
            val fieldSymbol = dataMembers(fieldIndex)
            val width = fieldSymbol.kind.width.toInt
            val signed = fieldSymbol.kind.isSigned
            val lsb = (dataMembers.iterator map { _.kind.width.toInt })
              .scanLeft(0)(_ + _)
              .drop(fieldIndex)
              .next()
            TypeAssigner(
              ExprInt(signed, width, known.value.extract(lsb, width, signed)) withLoc tree.loc
            )
          case other => TypeAssigner(e.copy(expr = other) withLoc tree.loc)
        }
      }

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Substitute known constants of scalar types
    case ExprSym(symbol) if symbol.kind.underlying.isNumeric =>
      // Drop bindings which contain non-numeric symbols, as operators change
      // over them. This only happens prior to SplitStructs and LowerVectors.
      val bs: Symbol => Option[Expr] =
        bindings.top.get(_).filterNot(_ existsAll { case tree => !tree.tpe.isNumeric })
      @tailrec // Recursively replace with bound values
      def simplify(expr: Expr): Expr = (expr substitute bs).simplify match {
        case simplified: ExprInt => simplified
        case simplified          => if (simplified eq expr) expr else simplify(simplified)
      }
      bs(symbol) map simplify match {
        // If the value is known, replace the ref
        case Some(expr: ExprInt) => expr
        // If the current ref is a temporary, and the replacement is simply
        // another symbol, replace the temporary with the other symbol
        case Some(expr: ExprSym) if symbol.attr.tmp.isSet => expr
        // Replace temporaries explicitly marked to be dropped, even if they
        // are bound to a complex expression
        case Some(expr) if temporariesToDrop(symbol) && symbol.kind == expr.tpe => expr
        //
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
