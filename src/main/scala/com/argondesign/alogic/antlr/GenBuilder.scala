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
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object GenBuilder extends BaseBuilder[GenContext, Gen] {

  def apply(ctx: GenContext)(implicit cc: CompilerContext): Gen = {
    object GenItemVisitor extends AlogicScalarVisitor[Tree] {
      // format: off
      override def visitGenItemGen(ctx: GenItemGenContext): Tree = GenBuilder(ctx.gen)
      override def visitGenItemDesc(ctx: GenItemDescContext): Tree = DescBuilder(ctx.desc)
      override def visitGenItemStmt(ctx: GenItemStmtContext): Tree = StmtBuilder(ctx.stmt)
      override def visitGenItemCase(ctx: GenItemCaseContext): Tree = CaseBuilder(ctx.kase)
      override def visitGenItemEnt(ctx: GenItemEntContext): Tree = EntBuilder(ctx.ent)
      override def visitGenItemRec(ctx: GenItemRecContext): Tree = RecBuilder(ctx.rec)
      // format: on
    }

    object GinitVisitor extends AlogicScalarVisitor[StmtDesc] {
      override def visitGinit(ctx: GinitContext): StmtDesc = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val spec = ExprBuilder(ctx.expr(0))
        val init = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        val desc = DescGen(ident, spec, init) withLoc loc
        StmtDesc(desc) withLoc loc
      }
    }

    object GenVisitor extends AlogicScalarVisitor[Gen] {
      override def visitGen(ctx: GenContext): Gen = visit(ctx.generate)

      override def visitGenIf(ctx: GenIfContext): Gen = {
        val thenCond = ExprBuilder(ctx.thenCond)
        val thenItems = GenItemVisitor(ctx.thenItems.genitem)
        val elifConds = ExprBuilder(ctx.elifCond)
        val elifItemss = List from {
          ctx.elifItems.iterator.asScala map { c =>
            GenItemVisitor(c.genitem)
          }
        }
        val elseItems = if (ctx.elseItems != null) GenItemVisitor(ctx.elseItems.genitem) else Nil

        @tailrec
        def loop(conds: List[Expr], thenItemss: List[List[Tree]], elseItems: List[Tree]): GenIf = {
          require(conds.length == thenItemss.length)
          if (conds.isEmpty) {
            elseItems.head match {
              case gen: GenIf => gen
              case _          => unreachable
            }
          } else {
            val genIf = GenIf(conds.head, thenItemss.head, elseItems) withLoc {
              ctx.loc.copy(point = conds.head.loc.start)
            }
            loop(conds.tail, thenItemss.tail, List(genIf))
          }
        }

        loop((thenCond :: elifConds).reverse, (thenItems :: elifItemss).reverse, elseItems)
      }

      override def visitGenFor(ctx: GenForContext): Gen = {
        val inits = GinitVisitor(ctx.ginits.ginit)
        val cond = ExprBuilder(ctx.expr)
        val steps = StmtBuilder(ctx.lsteps.lstep)
        val body = GenItemVisitor(ctx.genitems.genitem)
        GenFor(inits, cond, steps, body) withLoc ctx.loc
      }

      override def visitGenRange(ctx: GenRangeContext): Gen = {
        val inits = {
          val ident = IdentBuilder(ctx.IDENTIFIER)
          val spec = ExprBuilder(ctx.expr(0))
          val init = ExprNum(false, 0) withLoc ident.loc
          val loc = ctx.loc.copy(point = ident.loc.start)
          val desc = DescGen(ident, spec, init) withLoc loc
          List(StmtDesc(desc) withLoc loc)
        }
        val end = ExprBuilder(ctx.expr(1))
        val body = GenItemVisitor(ctx.genitems.genitem)
        GenRange(inits, ctx.op, end, body) withLoc ctx.loc
      }
    }

    GenVisitor(ctx)
  }
}
