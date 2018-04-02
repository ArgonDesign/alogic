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

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.Connect
import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.ast.Trees.Function
import com.argondesign.alogic.ast.Trees.Instance
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Locationed

import scala.collection.JavaConverters._

object EntityBuilder extends BaseBuilder[EntityContext, Entity] {

  // A few helper to use locally
  private case class AutoInstance(entity: Entity) extends Locationed
  private case class FenceBlock(stmts: StmtBlock) extends Locationed
  private case class VerbatimBlock(lang: String, text: String) extends Locationed

  def apply(ctx: EntityContext)(implicit cc: CompilerContext): Entity = {
    object EntityContentVisitor extends AlogicScalarVisitor[AnyRef] {
      override def visitEntityContentInstance(ctx: EntityContentInstanceContext) = {
        val c = ctx.instance

        val name = c.IDENTIFIER(0).toIdent
        val module = c.IDENTIFIER(1).toIdent

        val paramNames = c.param_assigns.IDENTIFIER.asScala.toList map { _.text }
        val paramExprs = ExprBuilder(c.param_assigns.expr)

        Instance(name, module, paramNames, paramExprs) withLoc ctx.loc
      }

      override def visitEntityContentConnect(ctx: EntityContentConnectContext) = {
        ConnectBuilder(ctx.connect)
      }

      override def visitEntityContentEntity(ctx: EntityContentEntityContext) = {
        val entity = EntityBuilder(ctx.entity)
        if (Option(ctx.autoinst).isDefined) {
          AutoInstance(entity) withLoc ctx.loc
        } else {
          entity
        }
      }

      override def visitEntityContentFenceBlock(ctx: EntityContentFenceBlockContext) = {
        // We put the body in a block in case there are multiple fence blocks, which we will check later
        val stmt = StmtBlock(StmtBuilder(ctx.block.statement)) withLoc ctx.loc
        FenceBlock(stmt) withLoc ctx.loc
      }

      override def visitEntityContentFunction(ctx: EntityContentFunctionContext) = {
        val ident = ctx.IDENTIFIER.toIdent
        val stmts = StmtBuilder(ctx.block.statement)
        if (ctx.attr != null) {
          ident withAttr AttrBuilder(ctx.attr)
        }
        Function(ident, stmts) withLoc ctx.loc
      }

      override def visitEntityContentVerbatimBlock(ctx: EntityContentVerbatimBlockContext) = {
        VerbatimBlock(ctx.IDENTIFIER, ctx.VERBATIMBODY) withLoc ctx.loc
      }
    }

    object EntityVisitor extends AlogicScalarVisitor[Entity] {
      override def visitEntity(ctx: EntityContext) = {
        val ident = ctx.IDENTIFIER.toIdent
        val decls = DeclBuilder(ctx.decl)
        val contents = EntityContentVisitor(ctx.entity_content)
        val instances = contents collect {
          case x: Instance          => x
          case AutoInstance(entity) => Instance(entity.ref, entity.ref, Nil, Nil) withLoc entity.loc
        }
        val connects = contents collect { case x: Connect           => x }
        val functions = contents collect { case x: Function         => x }
        val fenceBlocks = contents collect { case FenceBlock(block) => block }
        val entities = contents collect {
          case x: Entity            => x
          case AutoInstance(entity) => entity
        }
        val verbatim = {
          val blocks = contents collect { case x: VerbatimBlock          => x }
          val blockMap = blocks groupBy { case VerbatimBlock(lang, text) => lang }
          blockMap mapValues { list =>
            list map { _.text.tail.init } mkString "\n"
          }
        }

        if (ctx.attr != null) {
          ident withAttr AttrBuilder(ctx.attr)
        }

        Entity(
          ident,
          decls,
          instances,
          connects,
          functions,
          Nil,
          fenceBlocks,
          entities,
          verbatim
        ) withLoc ctx.loc withVariant ctx.variant.text
      }
    }

    EntityVisitor(ctx)
  }

}
