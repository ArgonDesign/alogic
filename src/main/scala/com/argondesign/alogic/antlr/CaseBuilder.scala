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
// Build a Case AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object CaseBuilder extends BaseBuilder[Case_clauseContext, Case] {

  private def makeStmtList(stmt: Stmt): List[Stmt] = stmt match {
    case StmtBlock(ss) => ss
    case s             => List(s)
  }

  def apply(ctx: Case_clauseContext)(implicit cc: CompilerContext): Case = {
    object Visitor extends AlogicScalarVisitor[Case] {
      override def visitCaseRegular(ctx: CaseRegularContext) = {
        val cond = ExprBuilder(ctx.commaexpr.expr)
        val stmts = makeStmtList(StmtBuilder(ctx.statement))
        CaseRegular(cond, stmts) withLoc ctx.loc
      }
      override def visitCaseDefault(ctx: CaseDefaultContext) = {
        val stmts = makeStmtList(StmtBuilder(ctx.statement))
        CaseDefault(stmts) withLoc ctx.loc
      }
      override def visitCaseGen(ctx: CaseGenContext) = {
        CaseGen(GenBuilder(ctx.generate)) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }
}
