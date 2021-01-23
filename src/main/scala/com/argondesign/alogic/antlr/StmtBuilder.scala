////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a Stmt AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.SourceContext
import org.antlr.v4.runtime.ParserRuleContext

object StmtBuilder extends BaseBuilder[ParserRuleContext, Stmt] {

  private def stmts(stmt: Stmt): List[Stmt] = stmt match {
    case StmtBlock(ss) => ss
    case s             => List(s)
  }

  def apply(ctx: ParserRuleContext)(implicit mb: MessageBuffer, sc: SourceContext): Stmt = {
    object Visitor extends AlogicScalarVisitor[Stmt] { self =>
      override def visitStmtDesc(ctx: StmtDescContext): Stmt =
        StmtSplice(DescBuilder(ctx.desc)) withLoc ctx.loc

      override def visitStmtImport(ctx: StmtImportContext): Stmt =
        StmtSplice(ImportBuilder(ctx.imprt)) withLoc ctx.loc

      override def visitStmtUsing(ctx: StmtUsingContext): Stmt =
        StmtSplice(UsingBuilder(ctx.usng)) withLoc ctx.loc

      override def visitStmtFrom(ctx: StmtFromContext): Stmt =
        StmtSplice(FromBuilder(ctx.from)) withLoc ctx.loc

      override def visitStmtAssertion(ctx: StmtAssertionContext): Stmt =
        StmtSplice(AssertionBuilder(ctx.assertion)) withLoc ctx.loc

      override def visitStmtBlock(ctx: StmtBlockContext): Stmt =
        StmtBlock(visit(ctx.stmt)) withLoc ctx.loc

      override def visitStmtIf(ctx: StmtIfContext): Stmt = {
        val cond = ExprBuilder(ctx.expr)
        val thenStmts = stmts(visit(ctx.thenStmt))
        val elseStmts = if (ctx.elseStmt == null) {
          Nil
        } else {
          // Wrap it in a StmtBlock. This indicates an explicit 'else' was
          // written even if it's empty or 'gen' expands to empty
          List(StmtBlock(stmts(visit(ctx.elseStmt))) withLoc ctx.elseStmt.loc)
        }
        StmtIf(cond, thenStmts, elseStmts) withLoc ctx.loc
      }

      override def visitStmtCase(ctx: StmtCaseContext): Stmt =
        StmtCase(ExprBuilder(ctx.expr), CaseBuilder(ctx.kase)) withLoc ctx.loc

      override def visitStmtLoop(ctx: StmtLoopContext): Stmt =
        StmtLoop(visit(ctx.stmt)) withLoc ctx.loc

      override def visitStmtDo(ctx: StmtDoContext): Stmt =
        StmtDo(ExprBuilder(ctx.expr), visit(ctx.stmt)) withLoc ctx.loc

      override def visitStmtWhile(ctx: StmtWhileContext): Stmt =
        StmtWhile(ExprBuilder(ctx.expr), visit(ctx.stmt)) withLoc ctx.loc

      override def visitStmtFor(ctx: StmtForContext): Stmt = {
        val inits = if (ctx.linits != null) visit(ctx.linits.linit) else Nil
        val cond = if (ctx.expr != null) Some(ExprBuilder(ctx.expr)) else None
        val steps = if (ctx.lsteps != null) visit(ctx.lsteps.lstep) else Nil
        val body = visit(ctx.stmt)
        StmtFor(inits, cond, steps, body) withLoc ctx.loc
      }

      override def visitStmtLet(ctx: StmtLetContext): Stmt =
        StmtLet(visit(ctx.linits.linit), stmts(visit(ctx.stmt))) withLoc ctx.loc

      override def visitStmtFence(ctx: StmtFenceContext): Stmt =
        StmtFence() withLoc ctx.loc

      override def visitStmtBreak(ctx: StmtBreakContext): Stmt =
        StmtBreak() withLoc ctx.loc

      override def visitStmtContinue(ctx: StmtContinueContext): Stmt =
        StmtContinue() withLoc ctx.loc

      override def visitStmtGoto(ctx: StmtGotoContext): Stmt =
        StmtGoto(ExprBuilder(ctx.expr)) withLoc ctx.loc

      override def visitStmtReturn(ctx: StmtReturnContext): Stmt = {
        val comb = sc match {
          case SourceContext.FuncComb => true
          case SourceContext.FuncCtrl => false
          case SourceContext.Unknown =>
            throw Ice("Cannot parse 'return' statement without source context")
          case _ =>
            mb.error(ctx.loc, "'return' statement not inside function"); false
        }
        StmtReturn(comb, Option.when(ctx.expr != null)(ExprBuilder(ctx.expr))) withLoc ctx.loc
      }

      override def visitStmtAssign(ctx: StmtAssignContext): Stmt =
        StmtAssign(ExprBuilder(ctx.expr(0)), ExprBuilder(ctx.expr(1))) withLoc {
          ctx.loc.copy(point = ctx.point.getStartIndex)
        }

      override def visitStmtUpdate(ctx: StmtUpdateContext): Stmt =
        StmtUpdate(
          ExprBuilder(ctx.expr(0)),
          ctx.ASSIGNOP.txt.init,
          ExprBuilder(ctx.expr(1))
        ) withLoc {
          ctx.loc.copy(point = ctx.ASSIGNOP.getStartIndex)
        }

      override def visitStmtPost(ctx: StmtPostContext): Stmt =
        StmtPost(ExprBuilder(ctx.expr), ctx.op.txt) withLoc ctx.loc

      override def visitStmtExpr(ctx: StmtExprContext): Stmt =
        StmtExpr(ExprBuilder(ctx.expr)) withLoc ctx.loc

      override def visitStmtWait(ctx: StmtWaitContext): Stmt = {
        val cond = if (ctx.expr != null) {
          ExprBuilder(ctx.expr)
        } else {
          ExprInt(false, 1, 0) withLoc ctx.loc
        }
        StmtWait(cond) withLoc ctx.loc
      }

      override def visitLoopInitAssign(ctx: LoopInitAssignContext): Stmt =
        StmtAssign(ExprBuilder(ctx.expr(0)), ExprBuilder(ctx.expr(1))) withLoc {
          ctx.loc.copy(point = ctx.point.getStartIndex)
        }

      override def visitLoopInitDesc(ctx: LoopInitDescContext): Stmt = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val spec = ExprBuilder(ctx.expr(0))
        val init = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        val desc = DescVar(ident, Nil, spec, Some(init)) withLoc loc
        StmtSplice(desc) withLoc ctx.loc
      }

      override def visitLoopStepAssign(ctx: LoopStepAssignContext): Stmt =
        StmtAssign(ExprBuilder(ctx.expr(0)), ExprBuilder(ctx.expr(1))) withLoc {
          ctx.loc.copy(point = ctx.point.getStartIndex)
        }

      override def visitLoopStepUpdate(ctx: LoopStepUpdateContext): Stmt =
        StmtUpdate(
          ExprBuilder(ctx.expr(0)),
          ctx.ASSIGNOP.txt.init,
          ExprBuilder(ctx.expr(1))
        ) withLoc {
          ctx.loc.copy(point = ctx.ASSIGNOP.getStartIndex)
        }

      override def visitLoopStepPost(ctx: LoopStepPostContext): Stmt =
        StmtPost(ExprBuilder(ctx.expr), ctx.op.txt) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
