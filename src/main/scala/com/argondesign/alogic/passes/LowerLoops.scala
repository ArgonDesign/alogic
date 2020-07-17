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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.TypeCombStmt
import com.argondesign.alogic.core.Types.TypeCtrlStmt
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class LowerLoops(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // TODO: Add back correct skip condition
  override def skip(tree: Tree): Boolean = tree match {
    case _: Decl      => true
    case _: Expr      => true
    case _: EntAssign => true
    case _            => false

  }

  // Stack of statements to replace continue statements with
  private[this] val continueRewrites = mutable.Stack[Option[() => Tree]]()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case _: StmtLoop => continueRewrites.push(None)

      case StmtDo(cond, _) =>
        continueRewrites.push(Some { () =>
          StmtIf(cond, List(StmtContinue()), List(StmtBreak()))
        })

      case StmtWhile(cond, _) =>
        continueRewrites.push(Some { () =>
          StmtIf(cond, List(StmtContinue()), List(StmtBreak()))
        })

      case StmtFor(_, Some(cond), step, _) =>
        continueRewrites.push(Some { () =>
          Thicket(step :+ StmtIf(cond, List(StmtContinue()), List(StmtBreak())))
        })

      case StmtFor(_, None, step, _) =>
        continueRewrites.push(Some { () =>
          Thicket(step :+ StmtContinue())
        })

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {

    case _: StmtContinue if continueRewrites.top.isDefined => {
      val create = continueRewrites.top.get
      create() regularize tree.loc
    }

    case _: StmtLoop => {
        tree
      } tap { _ =>
        continueRewrites.pop()
      }

    case StmtDo(cond, body) => {
        val test = StmtIf(cond, List(StmtFence()), List(StmtBreak()))
        StmtLoop(body :+ test) regularize tree.loc
      } tap { _ =>
        continueRewrites.pop()
      }

    case StmtWhile(cond, body) => {
        val test = StmtIf(cond, List(StmtFence()), List(StmtBreak()))
        val loop = StmtLoop(body :+ test)
        StmtIf(cond, List(loop), Nil) regularize tree.loc
      } tap { _ =>
        continueRewrites.pop()
      }

    case StmtFor(inits, Some(cond), steps, body) => {
        val test = StmtIf(cond, List(StmtFence()), List(StmtBreak()))
        val loop = StmtLoop(body ::: (steps :+ test))
        Thicket(inits :+ StmtIf(cond, List(loop), Nil)) regularize tree.loc
      } tap { _ =>
        continueRewrites.pop()
      }

    case StmtFor(inits, None, steps, body) => {
        val extra = body.last.tpe match {
          case TypeCtrlStmt => steps
          case TypeCombStmt => steps :+ StmtFence()
          case _            => unreachable
        }
        Thicket(inits :+ StmtLoop(body ::: extra)) regularize tree.loc
      } tap { _ =>
        continueRewrites.pop()
      }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(continueRewrites.isEmpty)

    // $COVERAGE-OFF$ Debug code
    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "lost type")
      case node: StmtFor              => cc.ice(node, "for statement remains after LowerLoops")
      case node: StmtDo               => cc.ice(node, "do statement remains after LowerLoops")
      case node: StmtWhile            => cc.ice(node, "while statement remains after LowerLoops")
    }
    // $COVERAGE-ON$
  }

}

object LowerLoops extends PairTransformerPass {
  val name = "lower-loops"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new LowerLoops
    (transformer(decl), transformer(defn))
  }

}
