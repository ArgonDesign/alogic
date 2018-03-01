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
import com.argondesign.alogic.antlr.AlogicParser.DefaultCaseContext
import com.argondesign.alogic.antlr.AlogicParser.LoopInitAssignContext
import com.argondesign.alogic.antlr.AlogicParser.LoopInitDeclContext
import com.argondesign.alogic.antlr.AlogicParser.NormalCaseContext
import com.argondesign.alogic.antlr.AlogicParser.StatementAssignmentContext
import com.argondesign.alogic.antlr.AlogicParser.StatementLoopContext
import com.argondesign.alogic.antlr.AlogicParser.StmtAssignContext
import com.argondesign.alogic.antlr.AlogicParser.StmtBlockContext
import com.argondesign.alogic.antlr.AlogicParser.StmtBreakContext
import com.argondesign.alogic.antlr.AlogicParser.StmtCaseContext
import com.argondesign.alogic.antlr.AlogicParser.StmtDeclContext
import com.argondesign.alogic.antlr.AlogicParser.StmtDoContext
import com.argondesign.alogic.antlr.AlogicParser.StmtDollarCommentContext
import com.argondesign.alogic.antlr.AlogicParser.StmtExprContext
import com.argondesign.alogic.antlr.AlogicParser.StmtFenceContext
import com.argondesign.alogic.antlr.AlogicParser.StmtForContext
import com.argondesign.alogic.antlr.AlogicParser.StmtGotoContext
import com.argondesign.alogic.antlr.AlogicParser.StmtIfContext
import com.argondesign.alogic.antlr.AlogicParser.StmtLetContext
import com.argondesign.alogic.antlr.AlogicParser.StmtLoopContext
import com.argondesign.alogic.antlr.AlogicParser.StmtPostContext
import com.argondesign.alogic.antlr.AlogicParser.StmtReturnContext
import com.argondesign.alogic.antlr.AlogicParser.StmtUpdateContext
import com.argondesign.alogic.antlr.AlogicParser.StmtWhileContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.CaseClause
import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Stmt
import com.argondesign.alogic.ast.Trees.StmtAssign
import com.argondesign.alogic.ast.Trees.StmtBlock
import com.argondesign.alogic.ast.Trees.StmtBreak
import com.argondesign.alogic.ast.Trees.StmtCase
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
import com.argondesign.alogic.ast.Trees.StmtRead
import com.argondesign.alogic.ast.Trees.StmtReturn
import com.argondesign.alogic.ast.Trees.StmtUpdate
import com.argondesign.alogic.ast.Trees.StmtWhile
import com.argondesign.alogic.ast.Trees.StmtWrite
import com.argondesign.alogic.core.CompilerContext

import org.antlr.v4.runtime.ParserRuleContext

object StmtBuilder extends BaseBuilder[ParserRuleContext, Stmt] {

  def apply(ctx: ParserRuleContext)(implicit cc: CompilerContext): Stmt = {
    object Visitor extends AlogicScalarVisitor[Stmt] { self =>
      // Block
      override def visitBlock(ctx: BlockContext) = {
        StmtBlock(visit(ctx.statement)) withLoc ctx.loc
      }

      // Proxy nodes
      override def visitStatementAssignment(ctx: StatementAssignmentContext) = visit(ctx.assignment)
      override def visitStatementLoop(ctx: StatementLoopContext) = visit(ctx.loop)

      override def visitStmtDecl(ctx: StmtDeclContext) = {
        StmtDecl(DeclBuilder(ctx.decl)) withLoc ctx.loc
      }

      override def visitStmtAssign(ctx: StmtAssignContext) = {
        StmtAssign(ExprBuilder(ctx.expr(0)), ExprBuilder(ctx.expr(1))) withLoc ctx.loc
      }

      override def visitStmtUpdate(ctx: StmtUpdateContext) = {
        StmtUpdate(ExprBuilder(ctx.expr(0)), ctx.ASSIGNOP.text.init, ExprBuilder(ctx.expr(1))) withLoc ctx.loc
      }

      override def visitStmtPost(ctx: StmtPostContext) = {
        StmtPost(ExprBuilder(ctx.expr), ctx.op) withLoc ctx.loc
      }

      override def visitStmtDollarComment(ctx: StmtDollarCommentContext) = {
        StmtDollarComment(ctx.STRING.text.tail.init) withLoc ctx.loc
      }

      override def visitStmtLet(ctx: StmtLetContext) = {
        val inits = visit(ctx.let.loop_init.loop_init_item)
        val loop = visit(ctx.loop)
        StmtLet(inits, loop) withLoc ctx.loc
      }

      override def visitStmtLoop(ctx: StmtLoopContext) = {
        val body = visit(ctx.block.statement)
        StmtLoop(body) withLoc ctx.loc
      }

      override def visitStmtDo(ctx: StmtDoContext) = {
        val cond = ExprBuilder(ctx.expr)
        val body = visit(ctx.block.statement)
        StmtDo(cond, body) withLoc ctx.loc
      }

      override def visitStmtWhile(ctx: StmtWhileContext) = {
        val cond = ExprBuilder(ctx.expr)
        val body = visit(ctx.block.statement)
        StmtWhile(cond, body) withLoc ctx.loc
      }

      override def visitStmtFor(ctx: StmtForContext) = {
        val inits = if (ctx.loop_init != null) visit(ctx.loop_init.loop_init_item) else Nil
        val cond = Option(ctx.expr) map { ExprBuilder(_) }
        val step = if (ctx.for_steps != null) visit(ctx.for_steps.step) else Nil
        val body = visit(ctx.block.statement)
        StmtFor(inits, cond, step, body) withLoc ctx.loc
      }

      override def visitStmtGoto(ctx: StmtGotoContext) = {
        StmtGoto(ctx.IDENTIFIER.toIdent) withLoc ctx.loc
      }

      override def visitStmtFence(ctx: StmtFenceContext) = {
        StmtFence() withLoc ctx.loc
      }

      override def visitStmtBreak(ctx: StmtBreakContext) = {
        StmtBreak() withLoc ctx.loc
      }

      override def visitStmtReturn(ctx: StmtReturnContext) = {
        StmtReturn() withLoc ctx.loc
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
          override def visitDefaultCase(ctx: DefaultCaseContext) = {
            // We put the body in a block in case there are multiple defaults, which we will check later
            val body = self(ctx.statement)
            val block = StmtBlock(List(body)) withLoc ctx.loc
            Some(block)
          }
        }

        object CaseVisitor extends AlogicScalarVisitor[Option[CaseClause]] {
          override val defaultResult = None
          override def visitNormalCase(ctx: NormalCaseContext) = {
            val cond = ExprBuilder(ctx.commaexpr.expr)
            val stmt = self(ctx.statement)
            Some(CaseClause(cond, stmt) withLoc ctx.loc)
          }
        }

        val value = ExprBuilder(ctx.expr)

        val clauseList = ctx.case_clause.asScala.toList

        val caseClauses = clauseList flatMap { CaseVisitor(_) }

        val defaultCase = clauseList flatMap { DefaultVisitor(_) }

        StmtCase(value, caseClauses, defaultCase) withLoc ctx.loc
      }

      override def visitStmtExpr(ctx: StmtExprContext) = {
        val stmt = ctx.expr.text match {
          case "read"  => StmtRead()
          case "write" => StmtWrite()
          case _       => StmtExpr(ExprBuilder(ctx.expr))
        }
        stmt withLoc ctx.loc
      }

      override def visitLoopInitAssign(ctx: LoopInitAssignContext) = {
        StmtAssign(ExprBuilder(ctx.expr(0)), ExprBuilder(ctx.expr(1))) withLoc ctx.loc
      }

      override def visitLoopInitDecl(ctx: LoopInitDeclContext) = {
        val ident = ctx.IDENTIFIER.toIdent
        val kind = TypeBuilder(ctx.kind)
        val init = ExprBuilder(ctx.expr)
        val decl = Decl(ident, kind, Some(init)) withLoc ctx.loc
        StmtDecl(decl) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
