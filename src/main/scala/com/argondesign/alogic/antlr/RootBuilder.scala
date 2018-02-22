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
// Build an Root AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.StartContext
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.core.CompilerContext

object RootBuilder extends BaseBuilder[StartContext, Root] {

  def apply(ctx: StartContext)(implicit cc: CompilerContext): Root = {
    val typeDefinitions = TypeDefinitionBuilder(ctx.type_definition)
    val entity = EntityBuilder(ctx.entity)

    Root(typeDefinitions, entity)
  }

}
