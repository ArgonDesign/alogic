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

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.typer.TypeAssigner

final class Fold(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  private val zero = BigInt(0)

  private def empty(stmts: List[Stmt]): Boolean = stmts forall {
    case _: StmtComment => true
    case _              => false
  }

  private def simplifyAssignmentSource(expr: Expr): Expr = expr.simplify match {
    // Drop pointless call to $unsigned/$signed
    case ExprCall(ExprSym(Symbol("$unsigned" | "$signed")), args) => args.head.expr
    case other                                                    => other
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Simplify expressions
    case expr: Expr => Some(expr.simplify)

    // Simplify expressions in assignments specially
    case StmtAssign(lhs, rhs) =>
      Some {
        TypeAssigner {
          StmtAssign(lhs.simplifyLValue, simplifyAssignmentSource(rhs)) withLoc tree.loc
        }
      }
    case StmtDelayed(lhs, rhs) =>
      Some {
        TypeAssigner {
          StmtDelayed(lhs.simplifyLValue, simplifyAssignmentSource(rhs)) withLoc tree.loc
        }
      }
    case EntConnect(lhs, rhss) =>
      Some {
        TypeAssigner {
          EntConnect(simplifyAssignmentSource(lhs), rhss map { _.simplifyLValue }) withLoc tree.loc
        }
      }

    // Fold 'if' with known conditions
    case StmtIf(cond, thenStmts, elseStmts) =>
      cond.value match {
        case Some(v) if v != 0 => Some(Thicket(walk(thenStmts)))
        case Some(v) if v == 0 => Some(Thicket(walk(elseStmts)))
        case None              => None
      }

    // Fold 'stall' with known conditions
    case StmtStall(cond) =>
      cond.value match {
        case Some(v) if v != 0 => Some(Stump)
        case Some(v) if v == 0 => cc.error(tree, "Stall condition is always true"); Some(tree)
        case None              => None
      }

    // TODO: Fold 'case' with known conditions

    // Drop type definitions, references to these will be folded by expr.simplify
    case _: DeclType => Some(Stump)
    case _: DefnType => Some(Stump)

    // Drop unsized consts, references to these will be folded by expr.simplify
    case DeclConst(symbol, _) if symbol.kind.underlying.isNum => Some(Stump)
    case DefnConst(symbol, _) if symbol.kind.underlying.isNum => Some(Stump)

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Remove all blocks (this will also remove empty blocks)
    case StmtBlock(stmts) => Thicket(stmts)

    // Simplify 'if' with empty branches
    case StmtIf(cond, ts, es) =>
      (empty(ts), empty(es)) match {
        case (true, true)  => Stump
        case (false, true) => TypeAssigner(StmtIf(cond, ts, Nil) withLoc tree.loc)
        case (true, false) => TypeAssigner(StmtIf((!cond).simplify, es, Nil) withLoc tree.loc)
        case _             => tree
      }

    // Remove empty 'case' statements
    case StmtCase(_, cases) if cases.iterator map { _.stmts } forall empty => Stump

    // Drop empty processes
    case EntCombProcess(stmts) if empty(stmts)          => Stump
    case EntClockedProcess(_, _, stmts) if empty(stmts) => Stump

    // Fail on known false assertions
    case AssertionAssert(cond, msgOpt) if cond.value contains zero =>
      val suffix = msgOpt match {
        case Some(msg) => ": " + msg;
        case None      => ""
      }
      cc.error(tree, s"Assertion is always false$suffix")
      tree

    //
    case _ => tree
  }
}

object Fold extends PairTransformerPass {
  val name = "fold"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (cc.fold(decl), cc.fold(defn))
}
