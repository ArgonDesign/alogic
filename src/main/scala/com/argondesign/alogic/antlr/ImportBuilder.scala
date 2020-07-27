////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build an Import AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object ImportBuilder extends BaseBuilder[ImprtContext, Import] {

  def apply(ctx: ImprtContext)(implicit mb: MessageBuffer, sc: SourceContext): Import = {
    object Visitor extends AlogicScalarVisitor[Import] {
      override def visitImportOne(ctx: ImportOneContext): Import =
        ImportOne(
          if (ctx.relative == null) 0 else ctx.relative.size,
          ExprBuilder(ctx.expr),
          Option.when(ctx.ident != null)(IdentBuilder(ctx.ident))
        ) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
