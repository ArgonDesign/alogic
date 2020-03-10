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
// Fold expressions and statements
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class Fold(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(body)         => body forall emptyStmt
    case StmtIf(_, eBody, tBody) => (eBody forall emptyStmt) && (tBody forall emptyStmt)
    case StmtCase(_, cases) =>
      cases forall {
        case CaseRegular(_, stmts) => stmts forall emptyStmt
        case CaseDefault(stmts)    => stmts forall emptyStmt
        case _: CaseGen            => unreachable
      }
    case _: StmtComment => true
    case _              => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Simplify expressions
    case expr: Expr => Some(expr.simplify)

    // Fold 'if' with known conditions
    case StmtIf(cond, thenStmts, elseStmts) =>
      cond.value match {
        case Some(v) if v != 0 => Some(Thicket(thenStmts map walk))
        case Some(v) if v == 0 => Some(Thicket(elseStmts map walk))
        case None              => None
      }

    // Fold 'stall' with known conditions
    case StmtStall(cond) =>
      cond.value match {
        case Some(v) if v != 0 => Some(Stump)
        case Some(v) if v == 0 => cc.error(tree, "Stall condition is always true"); Some(tree)
        case None              => None
      }

    // TODO: Fold StmtCase

    // Drop type definitions, references to these will be folded
    case _: DeclType => Some(Stump)
    case _: DefnType => Some(Stump)

    // Drop unsized consts,  references to these will be folded
    case DeclConst(symbol, _) if symbol.kind.underlying.isNum => Some(Stump)
    case DefnConst(symbol, _) if symbol.kind.underlying.isNum => Some(Stump)

    //
    case _ =>
      None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Remove empty if
    case StmtIf(_, thenStmts, elseStmts)
        if (thenStmts forall emptyStmt) && (elseStmts forall emptyStmt) =>
      Stump

    // Invert condition with empty else
    case StmtIf(cond, Nil, elseStmts) =>
      TypeAssigner(StmtIf((!cond).simplify, elseStmts, Nil) withLoc tree.loc)

    // Drop empty processes
    case EntCombProcess(stmts) if stmts forall emptyStmt       => Stump
    case EntClockedProcess(_, stmts) if stmts forall emptyStmt => Stump

    //
    case _ => tree
  }

}

object Fold extends PairTransformerPass {
  val name = "fold"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new Fold
    (transformer(decl), transformer(defn))
  }
}
