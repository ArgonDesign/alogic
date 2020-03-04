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
// Build an Root AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.RootContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.core.CompilerContext

object RootBuilder extends BaseBuilder[RootContext, Root] {
  def apply(ctx: RootContext)(implicit cc: CompilerContext): Root =
    Root(RizBuilder(ctx.riz)) withLoc ctx.loc
}
