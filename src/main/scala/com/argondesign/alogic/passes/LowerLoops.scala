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
// Rewrite do/while/for loops to 'loop' loops.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.util.FollowedBy

final class LowerLoops(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity   => false
    case _: Function => false
    case _: Stmt     => false
    case _: Case     => false
    case _           => true
  }

  // Stack of statements to replace continue statements with
  private[this] val continueRewrites = Stack[Option[() => Stmt]]()

  override def enter(tree: Tree): Unit = tree match {
    case _: StmtLoop => continueRewrites.push(None)

    case StmtDo(cond, _) => {
      continueRewrites.push(Some({ () =>
        StmtIf(cond, List(StmtContinue()), List(StmtBreak()))
      }))
    }

    case StmtWhile(cond, _) => {
      continueRewrites.push(Some({ () =>
        StmtIf(cond, List(StmtContinue()), List(StmtBreak()))
      }))
    }

    case StmtFor(_, Some(cond), step, _) => {
      continueRewrites.push(Some({ () =>
        StmtBlock(step :+ StmtIf(cond, List(StmtContinue()), List(StmtBreak())))
      }))
    }

    case StmtFor(_, None, step, _) => {
      continueRewrites.push(Some({ () =>
        StmtBlock(step :+ StmtContinue())
      }))
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {

    case _: StmtContinue if continueRewrites.top.isDefined => {
      val create = continueRewrites.top.get
      create() regularize tree.loc
    }

    case _: StmtLoop => {
      tree
    } followedBy {
      continueRewrites.pop()
    }

    case StmtDo(cond, body) => {
      val test = StmtIf(cond, List(StmtFence()), List(StmtBreak()))
      StmtLoop(body :+ test) regularize tree.loc
    } followedBy {
      continueRewrites.pop()
    }

    case StmtWhile(cond, body) => {
      val test = StmtIf(cond, List(StmtFence()), List(StmtBreak()))
      val loop = StmtLoop(body :+ test)
      StmtIf(cond, List(loop), Nil) regularize tree.loc
    } followedBy {
      continueRewrites.pop()
    }

    case StmtFor(inits, Some(cond), steps, body) => {
      val test = StmtIf(cond, List(StmtFence()), List(StmtBreak()))
      val loop = StmtLoop(body ::: (steps :+ test))
      StmtBlock(inits :+ StmtIf(cond, List(loop), Nil)) regularize tree.loc
    } followedBy {
      continueRewrites.pop()
    }

    case StmtFor(inits, None, steps, body) => {
      StmtBlock(inits :+ StmtLoop(body ::: steps)) regularize tree.loc
    } followedBy {
      continueRewrites.pop()
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(continueRewrites.isEmpty)

    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "lost type")
      case node: StmtFor              => cc.ice(node, "for statement remains after LowerLoops")
      case node: StmtDo               => cc.ice(node, "do statement remains after LowerLoops")
      case node: StmtWhile            => cc.ice(node, "while statement remains after LowerLoops")
    }
  }

}

object LowerLoops extends TreeTransformerPass {
  val name = "lower-loops"
  def create(implicit cc: CompilerContext) = new LowerLoops
}
