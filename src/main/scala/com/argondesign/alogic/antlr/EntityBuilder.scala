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
// Build an Entity AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import scala.collection.JavaConverters._

import com.argondesign.alogic.antlr.AlogicParser.EntityContentConnectContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentEntityContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentFenceBlockContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentFenceFunctionContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentFunctionContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentInstanceContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentVerbatimBlockContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContentVerilogFuctionContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Locationed
import com.argondesign.alogic.ast.Trees.Connect
import com.argondesign.alogic.ast.Trees.EntityIdent
import com.argondesign.alogic.ast.Trees.FunctionIdent
import com.argondesign.alogic.ast.Trees.InstanceIdent
import com.argondesign.alogic.ast.Trees.Stmt
import com.argondesign.alogic.core.CompilerContext

object EntityBuilder extends BaseBuilder[EntityContext, EntityIdent] {

  // A few helper to use locally
  private case class AutoInstanceIdent(entity: EntityIdent) extends Locationed
  private case class FenceBlock(stmts: List[Stmt]) extends Locationed
  private case class VerbatimBlock(lang: String, text: String) extends Locationed

  def apply(ctx: EntityContext)(implicit cc: CompilerContext): EntityIdent = {
    object EntityContentVisitor extends AlogicScalarVisitor[AnyRef] {
      override def visitEntityContentInstance(ctx: EntityContentInstanceContext) = {
        val c = ctx.instance

        val name = c.IDENTIFIER(0).text
        val module = c.IDENTIFIER(1).text

        val paramNames = c.param_assigns.IDENTIFIER.asScala.toList map { _.text }
        val paramExprs = ExprBuilder(c.param_assigns.expr)

        InstanceIdent(name, module, paramNames, paramExprs) withLoc ctx.loc
      }

      override def visitEntityContentConnect(ctx: EntityContentConnectContext) = {
        val c = ctx.connect

        val lhs = ExprBuilder(c.lhs)
        val rhss = ExprBuilder(c.rhs)

        Connect(lhs, rhss) withLoc ctx.loc
      }

      override def visitEntityContentEntity(ctx: EntityContentEntityContext) = {
        val entity = EntityBuilder(ctx.entity)
        if (Option(ctx.autoinst).isDefined) {
          AutoInstanceIdent(entity) withLoc ctx.loc
        } else {
          entity
        }
      }

      override def visitEntityContentFenceFunction(ctx: EntityContentFenceFunctionContext) = {
        cc.warning(ctx, "'void fence() {...}' function syntax is deprecated. Use a 'fence {...}' block instead")
        val stmts = StmtBuilder(ctx.block.statement)
        FenceBlock(stmts) withLoc ctx.loc
      }

      override def visitEntityContentFenceBlock(ctx: EntityContentFenceBlockContext) = {
        val stmts = StmtBuilder(ctx.block.statement)
        FenceBlock(stmts) withLoc ctx.loc
      }

      override def visitEntityContentFunction(ctx: EntityContentFunctionContext) = {
        val name = ctx.IDENTIFIER.text
        val stmts = StmtBuilder(ctx.block.statement)
        FunctionIdent(name, stmts) withLoc ctx.loc
      }

      override def visitEntityContentVerilogFuction(ctx: EntityContentVerilogFuctionContext) = {
        cc.warning(ctx, "'void verilog() {...}' function syntax is deprecated. " +
          "Use a 'verbatim verilog {...}' block instead")
        VerbatimBlock("verilog", ctx.VERBATIMBODY) withLoc ctx.loc
      }

      override def visitEntityContentVerbatimBlock(ctx: EntityContentVerbatimBlockContext) = {
        VerbatimBlock(ctx.IDENTIFIER, ctx.VERBATIMBODY) withLoc ctx.loc
      }
    }

    object EntityVisitor extends AlogicScalarVisitor[EntityIdent] {
      override def visitEntity(ctx: EntityContext) = {
        val name = ctx.IDENTIFIER.text
        val decls = DeclBuilder(ctx.entity_decl)
        val contents = EntityContentVisitor(ctx.entity_content)
        val instances = contents collect { case x: InstanceIdent => x }
        // TODO: autoinst
        val connects = contents collect { case x: Connect => x }
        val functions = contents collect { case x: FunctionIdent => x }
        val fenceStmts = {
          val blocks = contents collect { case x: FenceBlock => x }
          if (blocks.isEmpty) {
            Nil
          } else {
            for (excess <- blocks.tail) {
              cc.error(excess, s"Multiple fence blocks specified in entity ${name}")
            }
            blocks.head.stmts
          }
        }
        val entities = contents collect { case x: EntityIdent => x }
        val verbatim = {
          val blocks = contents collect { case x: VerbatimBlock => x }
          val blockMap = blocks groupBy { case VerbatimBlock(lang, text) => lang }
          blockMap mapValues { list => list map { _.text } mkString "\n" }
        }
        // TODO: check fsm/network/verilog variant subsets
        EntityIdent(name, decls, instances, connects, functions, fenceStmts, entities, verbatim) withLoc ctx.loc
      }
    }

    EntityVisitor(ctx)
  }

}
