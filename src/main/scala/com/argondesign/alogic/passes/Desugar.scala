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
// Desugar rewrites basic syntactic sugar elements and also
// brings the tree to a canonical form by removing nested block where
// they are redundant/disallowed.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable

final class Desugar(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    // "a++" rewritten as  "a = a + @zx(@bits(a), 1'b1)"
    case StmtPost(lhs, op) => {
      val refAtBits = cc.getGlobalTermSymbolRef("@bits", tree.loc)
      val refAtZx = cc.getGlobalTermSymbolRef("@zx", tree.loc)
      val width = ExprCall(refAtBits, List(lhs)) withLoc tree.loc
      val incr = ExprCall(refAtZx, List(width, ExprInt(false, 1, 1) withLoc tree.loc)) withLoc tree.loc
      val rhs = op match {
        case "++" => lhs + incr
        case "--" => lhs - incr
        case _    => unreachable
      }
      StmtAssign(lhs, rhs) withLoc tree.loc
    }

    // "a += b" rewritten as "a = a + b"
    case StmtUpdate(lhs, op, expr) => {
      val rhs = ExprBinary(lhs, op, expr) withLoc expr.loc
      StmtAssign(lhs, rhs) withLoc tree.loc
    }

    // "let(<init>) <loop>" rewritten as "<init> <loop>"
    case StmtLet(inits, body) => {
      Thicket(inits ::: body :: Nil)
    }

    // Strip redundant blocks
    case StmtBlock(single :: Nil) => single

    // Strip block around fence statements
    case entity: Entity => {
      entity.fenceStmts match {
        case List(StmtBlock(body)) =>
          entity.copy(fenceStmts = body) withLoc entity.loc withVariant entity.variant
        case _ => tree
      }
    }

    // Strip block around default case
    case StmtCase(cond, cases, List(StmtBlock(default))) => {
      StmtCase(cond, cases, default) withLoc tree.loc
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // Should have removed all StmtLet, StmtUpdate, StmtPost
    tree visit {
      case node: StmtLet => {
        cc.ice(node, s"Desugar should have removed all 'let' statements, but '${node}' remains")
      }
      case node: StmtUpdate => {
        cc.ice(node,
               s"Desugar should have removed all op = update statements, but '${node}' remains")
      }
      case node: StmtPost => {
        cc.ice(node,
               s"Desugar should have removed all postfix update statements, but '${node}' remains")
      }
    }
  }

}
