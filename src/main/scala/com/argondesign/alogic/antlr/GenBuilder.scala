////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a Gen AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.TypeGen
import org.antlr.v4.runtime.ParserRuleContext

object GenBuilder extends BaseBuilder[ParserRuleContext, Gen] {

  def apply(ctx: ParserRuleContext)(implicit cc: CompilerContext): Gen = {
    object GenItemVisitor extends AlogicScalarVisitor[Tree] {
      override def visitGenItemGen(ctx: GenItemGenContext): Tree = GenBuilder(ctx)

      override def visitGenItemStmt(ctx: GenItemStmtContext): Tree = StmtBuilder(ctx)
    }

    object GenVisitor extends AlogicScalarVisitor[Gen] { self =>
      override def visitGenerate(ctx: GenerateContext): Gen = visit(ctx.gen)

      override def visitGenIf(ctx: GenIfContext) = {
        val cond = ExprBuilder(ctx.expr)
        val thenItems = GenItemVisitor(ctx.thens)
        val elseItems = GenItemVisitor(ctx.elses)
        GenIf(cond, thenItems, elseItems) withLoc ctx.loc
      }

      override def visitGenFor(ctx: GenForContext) = {
        val inits = if (ctx.loop_init == null) { Nil } else {
          StmtBuilder(ctx.loop_init.loop_init_item) map {
            case stmt @ StmtDecl(decl: DeclIdent) =>
              StmtDecl(decl.copy(kind = TypeGen(decl.kind)) withLoc decl.loc) withLoc stmt.loc
            case other => other
          }
        }
        val cond = Option(ctx.expr) map { ExprBuilder(_) }
        val step = if (ctx.for_steps != null) StmtBuilder(ctx.for_steps.step) else Nil
        val body = GenItemVisitor(ctx.genitem)
        GenFor(inits, cond, step, body) withLoc ctx.loc
      }

      override def visitGenRange(ctx: GenRangeContext) = {
        val end = ExprBuilder(ctx.expr)
        val decl = {
          val kind = TypeGen(TypeBuilder(ctx.kind))
          val ident = ctx.IDENTIFIER.toIdent
          val loc = ctx.kind.loc.copy(point = ctx.op.loc.start, end = end.loc.end)
          DeclIdent(ident, kind, None) withLoc loc
        }
        val body = GenItemVisitor(ctx.genitem)
        GenRange(decl, ctx.op, end, body) withLoc ctx.loc
      }
    }

    GenVisitor(ctx)
  }

}
