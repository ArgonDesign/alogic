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
import scala.util.ChainingSyntax

object EntBuilder extends BaseBuilder[EntContext, List[Ent]] with ChainingSyntax {

  def apply(ctx: EntContext)(implicit cc: CompilerContext): List[Ent] = {
    object Visitor extends AlogicListVisitor[Ent] {
      override def visitEntDecl(ctx: EntDeclContext) = {
        List(EntDecl(DeclBuilder(ctx.decl)) withLoc ctx.loc)
      }

      override def visitEntDefn(ctx: EntDefnContext) = {
        List(EntDefn(DefnBuilder(ctx.defn)) withLoc ctx.loc)
      }

      override def visitEntEntity(ctx: EntEntityContext) = {
        val entity = EntityBuilder(ctx.entity)
        if (ctx.autoinst != null) {
          val (eIdent, iIdent) = entity.ref.asInstanceOf[Ident] pipe { ident =>
            (ident.copy(name = ident.name + "$") withLoc ident.loc withAttr ident.attr,
             ident.copy() withLoc ident.loc)
          }
          if (ctx.attr != null) {
            iIdent withAttr AttrBuilder(ctx.attr)
          }
          val ent = EntEntity(entity.copy(ref = eIdent) withLoc entity.loc) withLoc ctx.loc
          val inst = EntInstance(iIdent, eIdent, Nil, Nil) withLoc ctx.autoinst.loc
          List(ent, inst)
        } else {
          List(EntEntity(entity) withLoc ctx.loc)
        }
      }

      override def visitEntInstance(ctx: EntInstanceContext) = {
        val iIdent = IdentBuilder(ctx.ident(0))
        val eIdent = IdentBuilder(ctx.ident(1))

        val paramNames = ctx.param_assigns.IDENTIFIER.asScala.toList map { _.text }
        val paramExprs = ExprBuilder(ctx.param_assigns.expr)

        if (ctx.attr != null) {
          iIdent withAttr AttrBuilder(ctx.attr)
        }

        val loc = ctx.loc.copy(point = ctx.eqsign.getStartIndex)

        List(EntInstance(iIdent, eIdent, paramNames, paramExprs) withLoc loc)
      }

      override def visitEntConnect(ctx: EntConnectContext) = {
        val lhs = ExprBuilder(ctx.lhs)
        val rhss = ExprBuilder(ctx.rhs)
        List(EntConnect(lhs, rhss) withLoc ctx.loc)
      }

      override def visitEntFenceBlock(ctx: EntFenceBlockContext) = {
        // We put the body in a block in case there are multiple fence blocks, which we will check later
        val stmt = StmtBlock(StmtBuilder(ctx.block.statement)) withLoc ctx.loc
        List(EntCombProcess(List(stmt)) withLoc ctx.loc)
      }

      override def visitEntFunction(ctx: EntFunctionContext) = {
        val ident = IdentBuilder(ctx.ident)
        val stmts = StmtBuilder(ctx.block.statement)
        if (ctx.attr != null) {
          ident withAttr AttrBuilder(ctx.attr)
        }
        List(EntFunction(ident, stmts) withLoc ctx.loc)
      }

      override def visitEntVerbatimBlock(ctx: EntVerbatimBlockContext) = {
        List(EntVerbatim(ctx.IDENTIFIER, ctx.VERBATIM_BODY.text.tail.init) withLoc ctx.loc)
      }

      override def visitEntGen(ctx: EntGenContext) = {
        List(EntGen(GenBuilder(ctx.generate)) withLoc ctx.loc)
      }
    }

    Visitor(ctx)
  }
}
