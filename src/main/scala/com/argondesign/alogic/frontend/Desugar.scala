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

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable

final class Desugar(implicit cc: CompilerContext) extends TreeTransformer { namer =>

  override def transform(tree: Tree): Tree = tree match {
    // "a++" rewritten as  "a = a + @zx(@bits(a), 1'b1)"
    case StmtPost(lhs, op) => {
      val width = ExprAtCall("bits", List(lhs)) withLoc tree.loc
      val incr = ExprAtCall("zx", List(width, ExprInt(false, 1, 1) withLoc tree.loc)) withLoc tree.loc
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

    case _                        => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // Should have removed all StmtLet, StmtUpdate, StmtPost
    tree visit {
      case node: StmtLet => {
        cc.ice(node, s"Desugar should have removed all 'let' statements, but '${node}' remains")
      }
      case node: StmtUpdate => {
        cc.ice(node, s"Desugar should have removed all op = update statements, but '${node}' remains")
      }
      case node: StmtPost => {
        cc.ice(node, s"Desugar should have removed all postfix update statements, but '${node}' remains")
      }
    }
  }

}
