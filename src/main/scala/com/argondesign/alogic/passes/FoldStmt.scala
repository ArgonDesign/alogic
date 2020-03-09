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

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class FoldStmt(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: DefnEntity        => false
    case _: EntCombProcess    => false
    case _: EntClockedProcess => false
    case _: Stmt              => false
    case _: Case              => false
    case _                    => true
  }

  private def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(body)         => body forall emptyStmt
    case StmtIf(_, eBody, tBody) => (eBody forall emptyStmt) && (tBody forall emptyStmt)
    case StmtCase(_, cases) =>
      cases forall {
        case CaseRegular(_, stmts) => stmts forall emptyStmt
        case CaseDefault(stmts)    => stmts forall emptyStmt
        case _: CaseGen            => unreachable
      }
    case _: StmtExpr    => true
    case _: StmtComment => true
    case _              => false
  }

  override def transform(tree: Tree): Tree = tree match {
    // Remove empty if
    case StmtIf(_, Nil, Nil) => Stump

    // Invert condition with empty else
    case StmtIf(cond, Nil, elseStmts) =>
      walk(StmtIf(!cond, elseStmts, Nil) regularize tree.loc)

    case stmt @ StmtIf(cond, thenStmts, elseStmts) =>
      val simp = cond.simplify
      simp.value match {
        case Some(v) if v != 0    => Thicket(thenStmts)
        case Some(v) if v == 0    => Thicket(elseStmts)
        case None if simp eq cond => tree
        case None                 => TypeAssigner(stmt.copy(cond = simp) withLoc tree.loc)
      }

    case StmtStall(cond) =>
      val simp = cond.simplify
      simp.value match {
        case Some(v) if v != 0    => Stump
        case Some(v) if v == 0    => cc.error(tree, "Stall condition is always true"); tree
        case None if simp eq cond => tree
        case None                 => TypeAssigner(StmtStall(simp) withLoc tree.loc)
      }

    // TODO: Fold StmtCase

    // Drop empty processes
    case EntCombProcess(stmts) if stmts forall emptyStmt => Stump

    case EntClockedProcess(_, stmts) if stmts forall emptyStmt => Stump

    case _ => tree
  }

}

object FoldStmt extends EntityTransformerPass(declFirst = true) {
  val name = "fold-stms"
  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new FoldStmt
}
