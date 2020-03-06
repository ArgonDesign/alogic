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
// Fold statements
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class FoldStmt(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case defn: DefnEntity  => defn.combProcesses.isEmpty
    case _: EntCombProcess => false
    case _: Stmt           => false
    case _: Case           => false
    case _: Expr           => false
    case _                 => true
  }

  private[this] var bindingsMap: Map[Int, Bindings] = _

  private[this] val bindings = mutable.Stack[Bindings]()

  override def enter(tree: Tree): Option[Tree] = tree match {
    case EntCombProcess(stmts) =>
      bindingsMap = StaticEvaluation(StmtBlock(stmts))._1
      None

    case stmt @ StmtAssign(_, rhs) =>
      // Don't fold constants on the lhs TODO: fold the read ones...
      Some {
        bindings.push(bindingsMap.getOrElse(stmt.id, Bindings.empty))
        TypeAssigner(stmt.copy(rhs = walk(rhs).asInstanceOf[Expr]) withLoc tree.loc) tap { _ =>
          bindings.pop()
        }
      }

    case stmt: Stmt =>
      bindings.push(bindingsMap.getOrElse(stmt.id, Bindings.empty))
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

    case _ => None
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {
      // Substitute known constants
      case ExprSym(symbol) =>
        bindings.head get symbol map {
          _.simplify match {
            case known: ExprInt => known
            case _              => tree
          }
        } getOrElse tree

      // Fold if statements
      case StmtIf(cond, thenStmts, elseStmts) =>
        cond.value match {
          case Some(v) if v != 0 => Thicket(thenStmts) regularize tree.loc
          case Some(v) if v == 0 => Thicket(elseStmts) regularize tree.loc
          case None              => tree
        }

      // Fold stall statements
      case StmtStall(cond) =>
        cond.value match {
          case Some(v) if v != 0 => Thicket(Nil) regularize tree.loc
          case Some(v) if v == 0 =>
            cc.error(tree, "Stall condition is always true")
            tree
          case None => tree
        }

      // TODO: Fold case statements

      case _ => tree
    }

    if (tree.isInstanceOf[Stmt]) {
      bindings.pop()
    }

    result
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(bindings.isEmpty)
  }
}

object FoldStmt extends EntityTransformerPass(declFirst = true) {
  val name = "fold-stms"
  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new FoldStmt
}
