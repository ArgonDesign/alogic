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
// Build a TypeDefinition AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

import com.argondesign.alogic.antlr.AlogicParser.Type_definitionContext
import com.argondesign.alogic.antlr.AlogicParser.TypeDefinitionStructContext
import com.argondesign.alogic.antlr.AlogicParser.TypeDefinitionTypedefContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.TypeDefinition
import com.argondesign.alogic.ast.Trees.TypeDefinitionStruct
import com.argondesign.alogic.ast.Trees.TypeDefinitionTypedef
import com.argondesign.alogic.core.CompilerContext

object TypeDefinitionBuilder extends BaseBuilder[Type_definitionContext, TypeDefinition] {

  def apply(ctx: Type_definitionContext)(implicit cc: CompilerContext): TypeDefinition = {
    object Visitor extends AlogicScalarVisitor[TypeDefinition] {
      override def visitTypeDefinitionTypedef(ctx: TypeDefinitionTypedefContext) = {
        TypeDefinitionTypedef(ctx.IDENTIFIER, TypeBuilder(ctx.kind))
      }

      override def visitTypeDefinitionStruct(ctx: TypeDefinitionStructContext) = {
        val fields = ctx.field.asScala.toList map { ctx =>
          ctx.IDENTIFIER.text -> TypeBuilder(ctx.kind)
        }
        TypeDefinitionStruct(ctx.IDENTIFIER, ListMap(fields: _*))
      }
    }

    Visitor(ctx)
  }

}
