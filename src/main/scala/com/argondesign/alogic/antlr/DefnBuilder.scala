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
// Build a Definition AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.DefnContext
import com.argondesign.alogic.antlr.AlogicParser.DefnStructContext
import com.argondesign.alogic.antlr.AlogicParser.DefnTypedefContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.TypeStruct

import scala.jdk.CollectionConverters._

object DefnBuilder extends BaseBuilder[DefnContext, DefnIdent] {

  def apply(ctx: DefnContext)(implicit cc: CompilerContext): DefnIdent = {
    object Visitor extends AlogicScalarVisitor[DefnIdent] {
      override def visitDefnTypedef(ctx: DefnTypedefContext) = {
        DefnIdent(ctx.IDENTIFIER.toIdent, TypeBuilder(ctx.kind)) withLoc ctx.loc
      }

      override def visitDefnStruct(ctx: DefnStructContext) = {
        val fields = ctx.field.asScala.toList

        val fieldNames = fields map { _.IDENTIFIER.text }
        val fieldKinds = fields map { ctx =>
          TypeBuilder(ctx.kind)
        }

        val kind = TypeStruct(ctx.IDENTIFIER, fieldNames, fieldKinds)

        DefnIdent(ctx.IDENTIFIER.toIdent, kind) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
