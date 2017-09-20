////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Extract type definitions form parse tree.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import scala.collection.immutable.ListMap
import scala.collection.mutable

import alogic.Antlr4Conversions._

import org.antlr.v4.runtime.ParserRuleContext
import alogic.VScalarVisitor
import alogic.Message
import alogic.antlr.VParser._

object ExtractTypedefs {

  def apply(root: ParserRuleContext,
            initialTypedefs: scala.collection.Map[String, Type]): (Map[String, Type], ParserRuleContext) = {
    val typedefs = mutable.Map[String, Type]() ++ initialTypedefs

    var entityCtx: ParserRuleContext = null

    object TypeDefinitionExtractor extends VScalarVisitor[Unit] {
      override def defaultResult = Message.ice("Should be called with Start node")

      override def visitStart(ctx: StartContext) = {
        visit(ctx.typedefinition)
        // we save the parse tree node to save walking the whole parse tree again
        entityCtx = ctx.entity
      }

      override def visitTypedefinition(ctx: TypedefinitionContext) = {
        visit(Option(ctx.typedef))
        visit(Option(ctx.struct))
      }

      override def visitStruct(ctx: StructContext) = {
        val name = ctx.IDENTIFIER.text
        if (typedefs contains name) {
          Message.error(ctx, s"Repeated structure definition 'struct $name'")
        }
        val visitor = new KnownTypeVisitor(None, typedefs)
        val pairs = ctx.fields.toList map { c => c.IDENTIFIER.text -> visitor(c.known_type) }
        typedefs(name) = Struct(name, ListMap(pairs: _*))
      }

      override def visitTypedef(ctx: TypedefContext) = {
        val s = ctx.IDENTIFIER.text
        if (typedefs contains s) {
          Message.error(ctx, s"Repeated typedef '$s'")
        }
        val visitor = new KnownTypeVisitor(None, typedefs)
        typedefs(s) = visitor(ctx.known_type)
      }
    }

    TypeDefinitionExtractor(root)

    (typedefs.toMap, entityCtx)
  }
}
