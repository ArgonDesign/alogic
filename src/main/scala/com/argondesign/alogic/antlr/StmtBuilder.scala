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
// Build a Stmt AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import scala.collection.JavaConverters._

import com.argondesign.alogic.antlr.AlogicParser.BlockContext
import com.argondesign.alogic.antlr.AlogicParser.CombStmtAssignContext
import com.argondesign.alogic.antlr.AlogicParser.CombStmtDeclContext
import com.argondesign.alogic.antlr.AlogicParser.CombStmtDollarCommentContext
import com.argondesign.alogic.antlr.AlogicParser.CombStmtPostContext
import com.argondesign.alogic.antlr.AlogicParser.CombStmtUpdateContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtBreakContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtDoContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtFenceContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtForContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtGotoContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtLetContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtLoopContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtReturnContext
import com.argondesign.alogic.antlr.AlogicParser.CtrlStmtWhileContext
import com.argondesign.alogic.antlr.AlogicParser.DefaultCaseContext
import com.argondesign.alogic.antlr.AlogicParser.LoopInitDeclContext
import com.argondesign.alogic.antlr.AlogicParser.NormalCaseContext
import com.argondesign.alogic.antlr.AlogicParser.StmtBlockContext
import com.argondesign.alogic.antlr.AlogicParser.StmtCaseContext
import com.argondesign.alogic.antlr.AlogicParser.StmtExprContext
import com.argondesign.alogic.antlr.AlogicParser.StmtIfContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.DeclIdent
import com.argondesign.alogic.ast.Trees.Stmt
import com.argondesign.alogic.ast.Trees.StmtAssign
import com.argondesign.alogic.ast.Trees.StmtBlock
import com.argondesign.alogic.ast.Trees.StmtBreak
import com.argondesign.alogic.ast.Trees.StmtCase
import com.argondesign.alogic.ast.Trees.StmtCaseClause
import com.argondesign.alogic.ast.Trees.StmtDecl
import com.argondesign.alogic.ast.Trees.StmtDo
import com.argondesign.alogic.ast.Trees.StmtDollarComment
import com.argondesign.alogic.ast.Trees.StmtExpr
import com.argondesign.alogic.ast.Trees.StmtFence
import com.argondesign.alogic.ast.Trees.StmtFor
import com.argondesign.alogic.ast.Trees.StmtGoto
import com.argondesign.alogic.ast.Trees.StmtIf
import com.argondesign.alogic.ast.Trees.StmtLet
import com.argondesign.alogic.ast.Trees.StmtLoop
import com.argondesign.alogic.ast.Trees.StmtPost
import com.argondesign.alogic.ast.Trees.StmtReturn
import com.argondesign.alogic.ast.Trees.StmtUpdate
import com.argondesign.alogic.ast.Trees.StmtWhile
import com.argondesign.alogic.core.CompilerContext

import org.antlr.v4.runtime.ParserRuleContext

object StmtBuilder extends BaseBuilder[ParserRuleContext, Stmt] {

  def apply(ctx: ParserRuleContext)(implicit cc: CompilerContext): Stmt = {
    object Visitor extends AlogicScalarVisitor[Stmt] { self =>
      // Unambiguous combinatorial statements
      override def visitCombStmtDecl(ctx: CombStmtDeclContext) = {
        StmtDecl(DeclBuilder(ctx.decl)) withLoc ctx.loc
      }
      override def visitCombStmtAssign(ctx: CombStmtAssignContext) = {
        StmtAssign(ExprBuilder(ctx.expr(0)), ExprBuilder(ctx.expr(1))) withLoc ctx.loc
      }
      override def visitCombStmtUpdate(ctx: CombStmtUpdateContext) = {
        StmtUpdate(ExprBuilder(ctx.expr(0)), ctx.ASSIGNOP.text.init, ExprBuilder(ctx.expr(1))) withLoc ctx.loc
      }
      override def visitCombStmtPost(ctx: CombStmtPostContext) = {
        StmtPost(ExprBuilder(ctx.expr), ctx.op) withLoc ctx.loc
      }
      override def visitCombStmtDollarComment(ctx: CombStmtDollarCommentContext) = {
        StmtDollarComment(ctx.STRING) withLoc ctx.loc
      }

      // Unambiguous control statements
      override def visitCtrlStmtLoop(ctx: CtrlStmtLoopContext) = {
        val body = visit(ctx.block)
        StmtLoop(body) withLoc ctx.loc
      }
      override def visitCtrlStmtDo(ctx: CtrlStmtDoContext) = {
        val cond = ExprBuilder(ctx.expr)
        val body = visit(ctx.block)
        StmtDo(cond, body) withLoc ctx.loc
      }
      override def visitCtrlStmtWhile(ctx: CtrlStmtWhileContext) = {
        val cond = ExprBuilder(ctx.expr)
        val body = visit(ctx.block)
        StmtWhile(cond, body) withLoc ctx.loc
      }
      override def visitCtrlStmtFor(ctx: CtrlStmtForContext) = {
        val inits = visit(ctx.loop_init.loop_init_item)
        val cond = ExprBuilder(ctx.expr)
        val step = visit(ctx.step)
        val body = visit(ctx.block)
        StmtFor(inits, cond, step, body) withLoc ctx.loc
      }
      override def visitCtrlStmtLet(ctx: CtrlStmtLetContext) = {
        val inits = visit(ctx.loop_init.loop_init_item)
        val body = visit(ctx.ctrl_statement_loop)
        StmtLet(inits, body) withLoc ctx.loc
      }
      override def visitCtrlStmtGoto(ctx: CtrlStmtGotoContext) = {
        StmtGoto(ctx.IDENTIFIER) withLoc ctx.loc
      }
      override def visitCtrlStmtFence(ctx: CtrlStmtFenceContext) = {
        StmtFence() withLoc ctx.loc
      }
      override def visitCtrlStmtBreak(ctx: CtrlStmtBreakContext) = {
        StmtBreak() withLoc ctx.loc
      }
      override def visitCtrlStmtReturn(ctx: CtrlStmtReturnContext) = {
        StmtReturn() withLoc ctx.loc
      }

      // Ambiguous statements
      override def visitBlock(ctx: BlockContext) = {
        StmtBlock(visit(ctx.statement)) withLoc ctx.loc
      }

      override def visitStmtBlock(ctx: StmtBlockContext) = visit(ctx.block)

      override def visitStmtIf(ctx: StmtIfContext) = {
        val cond = ExprBuilder(ctx.expr)
        val thenStmt = visit(ctx.thenStmt)
        val elseStmt = visit(Option(ctx.elseStmt))
        StmtIf(cond, thenStmt, elseStmt) withLoc ctx.loc
      }

      override def visitStmtCase(ctx: StmtCaseContext) = {
        object DefaultVisitor extends AlogicScalarVisitor[Option[Stmt]] {
          override val defaultResult = None
          override def visitDefaultCase(ctx: DefaultCaseContext) = Some(self(ctx.statement))
        }

        object CaseVisitor extends AlogicScalarVisitor[Option[StmtCaseClause]] {
          override val defaultResult = None
          override def visitNormalCase(ctx: NormalCaseContext) = {
            val cond = ExprBuilder(ctx.commaexpr.expr)
            val stmt = self(ctx.statement)
            Some(StmtCaseClause(cond, stmt) withLoc ctx.loc)
          }
        }

        val value = ExprBuilder(ctx.expr)

        val clauseList = ctx.case_clause.asScala.toList

        val caseClauses = clauseList flatMap { CaseVisitor(_) }

        val defaultCase = clauseList flatMap { DefaultVisitor(_) } match {
          case Nil      => None
          case d :: Nil => Some(d)
          case _ => {
            cc.error(ctx, "More than one 'default' case item specified")
            None
          }
        }

        StmtCase(value, caseClauses, defaultCase) withLoc ctx.loc
      }

      override def visitStmtExpr(ctx: StmtExprContext) = {
        StmtExpr(ExprBuilder(ctx.expr)) withLoc ctx.loc
      }

      override def visitLoopInitDecl(ctx: LoopInitDeclContext) = {
        val name = ctx.IDENTIFIER.text
        val kind = TypeBuilder(ctx.kind)
        val init = ExprBuilder(ctx.expr)
        StmtDecl(DeclIdent(name, kind, Some(init))) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
