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
// Build a List[Ent] AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.EntFunction
import com.argondesign.alogic.ast.Trees.EntInstance
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

import scala.jdk.CollectionConverters._

object EntBuilder extends BaseBuilder[Entity_contentContext, List[Ent]] {

  def apply(ctx: Entity_contentContext)(implicit cc: CompilerContext): List[Ent] = {
    object Visitor extends AlogicScalarVisitor[List[Ent]] {
      override def visitEntityContentEntity(ctx: EntityContentEntityContext) = {
        val entity = EntEntity(EntityBuilder(ctx.entity)) withLoc ctx.loc
        if (ctx.autoinst != null) {
          val eIdent = entity.entity.ref.asInstanceOf[Ident]
          val iIdent = eIdent.copy() withLoc eIdent.loc
          if (ctx.attr != null) {
            iIdent withAttr AttrBuilder(ctx.attr)
          }
          val inst = EntInstance(iIdent, eIdent, Nil, Nil) withLoc ctx.autoinst.loc
          List(entity, inst)
        } else {
          List(entity)
        }
      }

      override def visitEntityContentInstance(ctx: EntityContentInstanceContext) = {
        val iIdent = ctx.IDENTIFIER(0).toIdent
        val eIdent = ctx.IDENTIFIER(1).toIdent

        val paramNames = ctx.param_assigns.IDENTIFIER.asScala.toList map { _.text }
        val paramExprs = ExprBuilder(ctx.param_assigns.expr)

        if (ctx.attr != null) {
          iIdent withAttr AttrBuilder(ctx.attr)
        }

        val loc = ctx.loc.copy(point = ctx.eqsign.getStartIndex)

        List(EntInstance(iIdent, eIdent, paramNames, paramExprs) withLoc loc)
      }

      override def visitEntityContentConnect(ctx: EntityContentConnectContext) = {
        val lhs = ExprBuilder(ctx.lhs)
        val rhss = ExprBuilder(ctx.rhs)
        List(EntConnect(lhs, rhss) withLoc ctx.loc)
      }

      override def visitEntityContentFenceBlock(ctx: EntityContentFenceBlockContext) = {
        // We put the body in a block in case there are multiple fence blocks, which we will check later
        val stmt = StmtBlock(StmtBuilder(ctx.block.statement)) withLoc ctx.loc
        List(EntCombProcess(List(stmt)) withLoc ctx.loc)
      }

      override def visitEntityContentFunction(ctx: EntityContentFunctionContext) = {
        val ident = ctx.IDENTIFIER.toIdent
        val stmts = StmtBuilder(ctx.block.statement)
        if (ctx.attr != null) {
          ident withAttr AttrBuilder(ctx.attr)
        }
        List(EntFunction(ident, stmts) withLoc ctx.loc)
      }

      override def visitEntityContentVerbatimBlock(ctx: EntityContentVerbatimBlockContext) = {
        List(EntVerbatim(ctx.IDENTIFIER, ctx.VERBATIM_BODY.text.tail.init) withLoc ctx.loc)
      }
    }

    Visitor(ctx)
  }
}
