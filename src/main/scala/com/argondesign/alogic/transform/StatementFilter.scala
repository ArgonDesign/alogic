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
// A Tree transformer that selectively keeps statements based on a predicate.
// The predciate is a partial function. If it is defined and 'true' for a node
// That node is definitely kept. Otherwise, if it is defined and 'false' for a
// node, that node is definitely removed. Nodes for which the predicate is not
// defined are kept based on whether they have any descendants which are kept.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.passes.RemoveRedundantBlocks
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class StatementFilter(p: PartialFunction[Stmt, Boolean])(implicit cc: CompilerContext)
    extends TreeTransformer {

  override val typed: Boolean = true

  val pf = p.lift

  val removeRedundantBlocks = new RemoveRedundantBlocks

  def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(body)         => body forall emptyStmt
    case StmtIf(_, eBody, tBody) => (eBody forall emptyStmt) && (tBody forall emptyStmt)
    case StmtCase(_, cases) => {
      cases forall {
        case CaseRegular(_, stmts) => stmts forall emptyStmt
        case CaseDefault(stmts)    => stmts forall emptyStmt
        case _: CaseGen            => unreachable
      }
    }
    case _: StmtExpr    => true
    case _: StmtComment => true
    case _              => false
  }

  override def transform(tree: Tree): Tree = tree match {
    case stmt: Stmt => {
      val reduced = (stmt rewrite removeRedundantBlocks).asInstanceOf[Stmt]
      reduced match {
        case StmtBlock(Nil) => TypeAssigner(Thicket(Nil) withLoc tree.loc)
        case _ => {
          pf(reduced) match {
            case Some(false)             => TypeAssigner(Thicket(Nil) withLoc tree.loc)
            case None if emptyStmt(stmt) => TypeAssigner(Thicket(Nil) withLoc tree.loc)
            case _                       => reduced
          }
        }
      }
    }

    case _ => tree
  }
}

object StatementFilter {

  def apply(p: PartialFunction[Stmt, Boolean])(implicit cc: CompilerContext): StatementFilter = {
    new StatementFilter(p)
  }
}
