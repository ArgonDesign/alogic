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

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object StatementFilter {
  def apply(p: PartialFunction[Stmt, Boolean])(implicit cc: CompilerContext): TreeTransformer =
    new StatelessTreeTransformer {
      private val pf = p.lift

      override def transform(tree: Tree): Tree = tree match {
        case stmt: Stmt =>
          pf(stmt) match {
            case Some(false) => Stump // Don't keep
            case Some(true)  => tree // Keep
            case None => // Remove empty/pure statements
              stmt match {
                case StmtBlock(Nil)        => Stump
                case StmtIf(_, Nil, Nil)   => Stump
                case StmtCase(_, Nil)      => Stump
                case StmtExpr(_: ExprCall) => tree // Assume non-pure, keep
                case _: StmtExpr           => Stump
                case _: StmtComment        => Stump
                case _                     => tree
              }
          }

        // Remove empty case
        case CaseDefault(Nil)    => Stump
        case CaseRegular(_, Nil) => Stump

        case _ => tree
      }
    }
}
