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
// Build an Entity AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

import scala.jdk.CollectionConverters._

object EntityBuilder extends BaseBuilder[EntityContext, Entity] {

  def apply(ctx: EntityContext)(implicit cc: CompilerContext): Entity = {
    object Visitor extends AlogicScalarVisitor[Entity] {
      override def visitEntity(ctx: EntityContext) = {
        val ref = ctx.IDENTIFIER.toIdent

        val ents = ctx.ent.asScala.toList flatMap { EntBuilder(_) }

        val variantAttr = Map("//variant" -> (ExprStr(ctx.variant.text) withLoc ctx.loc))
        if (ctx.attr != null) {
          ref withAttr AttrBuilder(ctx.attr) ++ variantAttr
        } else {
          ref withAttr variantAttr
        }

        Entity(ref, ents) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }
}
