////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Normalize control function control statements
// - Add implicit 'fence' in empty branches of 'if' and 'case' statements
// - Add default 'fence' if it does not exist in 'case' statements
// - Replace calls in tail position with 'goto'
// - Replace 'fence'/'break' in final control statements with 'return'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

final class NormalizeControl(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  private def convertBreak(stmts: List[Stmt]): List[Stmt] = stmts map convertBreak

  private def convertBreak(stmt: Stmt): Stmt = stmt match {
    // Convert 'break' to 'return
    case _: StmtBreak => TypeAssigner(StmtReturn(comb = false, None) withLoc stmt.loc)

    // Nested statements, convert each branch
    case StmtBlock(body) =>
      TypeAssigner(StmtBlock(convertBreak(body)) withLoc stmt.loc)
    case s @ StmtIf(_, ts, es) =>
      TypeAssigner {
        s.copy(thenStmts = convertBreak(ts), elseStmts = convertBreak(es)) withLoc stmt.loc
      }
    case s @ StmtCase(_, cases) =>
      val newCases = cases map {
        case c @ CaseRegular(_, stmts) =>
          TypeAssigner(c.copy(stmts = convertBreak(stmts)) withLoc c.loc)
        case c @ CaseDefault(stmts) =>
          TypeAssigner(c.copy(stmts = convertBreak(stmts)) withLoc c.loc)
        case _: CaseGen => unreachable
      }
      TypeAssigner(s.copy(cases = newCases) withLoc s.loc)

    // Leave alone the rest (including nested 'loop')
    case _ => stmt
  }

  private def convertFinal(stmts: List[Stmt]): List[Stmt] =
    stmts.init appended convertFinal(stmts.last)

  private def convertFinal(stmt: Stmt): Stmt = {
    require(stmt.tpe.isCtrlStmt)
    stmt match {
      // 'return' and 'goto' are OK
      case _: StmtReturn => stmt
      case _: StmtGoto   => stmt

      // Convert final 'fence' to 'return'
      case _: StmtFence => TypeAssigner(StmtReturn(comb = false, None) withLoc stmt.loc)
      // Convert final 'call' to 'goto' (tail call)
      case StmtExpr(expr) =>
        expr match {
          case ExprCall(t, Nil) => StmtGoto(ExprSym(t.tpe.asCtrlFunc.symbol)) regularize stmt.loc
          case _                => unreachable
        }

      // Convert 'break' in final loop to 'return'
      case StmtLoop(body) => TypeAssigner(StmtLoop(convertBreak(body)) withLoc stmt.loc)

      // Nested statements, convert each branch
      case StmtBlock(body) =>
        TypeAssigner(StmtBlock(convertFinal(body)) withLoc stmt.loc)
      case s @ StmtIf(_, ts, es) =>
        TypeAssigner {
          s.copy(thenStmts = convertFinal(ts), elseStmts = convertFinal(es)) withLoc stmt.loc
        }
      case s @ StmtCase(_, cases) =>
        val newCases = cases map {
          case c @ CaseRegular(_, stmts) =>
            TypeAssigner(c.copy(stmts = convertFinal(stmts)) withLoc c.loc)
          case c @ CaseDefault(stmts) =>
            TypeAssigner(c.copy(stmts = convertFinal(stmts)) withLoc c.loc)
          case _: CaseGen => unreachable
        }
        TypeAssigner(s.copy(cases = newCases) withLoc s.loc)

      // The rest are either invalid in final position of have been removed
      // by earlier passes
      case _ => unreachable
    }
  }

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Add implicit fences in empty branches
    ////////////////////////////////////////////////////////////////////////////

    case stmt @ StmtIf(_, Nil, _) if stmt.tpe.isCtrlStmt =>
      val fence = TypeAssigner(StmtFence() withLoc tree.loc)
      TypeAssigner(stmt.copy(thenStmts = fence :: Nil) withLoc tree.loc)

    case stmt @ StmtIf(_, _, Nil) if stmt.tpe.isCtrlStmt =>
      val fence = TypeAssigner(StmtFence() withLoc tree.loc)
      TypeAssigner(stmt.copy(elseStmts = fence :: Nil) withLoc tree.loc)

    case stmt: StmtCase if stmt.tpe.isCtrlStmt =>
      val fence = TypeAssigner(StmtFence() withLoc tree.loc)

      val newCases = new ListBuffer[Case]()

      @tailrec
      def loop(cases: List[Case], needsDefault: Boolean = true): List[Case] = cases match {
        case (c: CaseRegular) :: tail =>
          if (c.stmts.nonEmpty) {
            newCases append c
          } else {
            newCases append TypeAssigner(c.copy(stmts = fence :: Nil) withLoc c.loc)
          }
          loop(tail, needsDefault)
        case (c: CaseDefault) :: tail =>
          if (c.stmts.nonEmpty) {
            newCases append c
          } else {
            newCases append TypeAssigner(CaseDefault(fence :: Nil) withLoc c.loc)
          }
          loop(tail, false)
        case (_: CaseGen) :: _ => unreachable
        case Nil =>
          if (needsDefault) {
            newCases prepend TypeAssigner(CaseDefault(fence :: Nil) withLoc stmt.loc)
          }
          newCases.toList
      }

      TypeAssigner(stmt.copy(cases = loop(stmt.cases)) withLoc stmt.loc)

    ////////////////////////////////////////////////////////////////////////////
    // Normalize final statements in control functions
    ////////////////////////////////////////////////////////////////////////////

    case defn @ DefnFunc(symbol, _, body) if symbol.kind.isCtrlFunc =>
      TypeAssigner(defn.copy(body = convertFinal(body)) withLoc defn.loc)

    case _ => tree
  }

}

object NormalizeControl extends EntityTransformerPass(declFirst = true) {
  val name = "normalize-control"

  override protected def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || defn.asInstanceOf[DefnEntity].variant != EntityVariant.Fsm

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new NormalizeControl
}
