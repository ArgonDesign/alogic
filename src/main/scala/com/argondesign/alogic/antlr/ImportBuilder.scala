////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build an Import AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object ImportBuilder extends BaseBuilder[ImprtContext, Import] {

  def apply(ctx: ImprtContext)(implicit mb: MessageBuffer, sc: SourceContext): Import = {
    object Visitor extends AlogicScalarVisitor[Import] {
      override def visitImportOne(ctx: ImportOneContext): Import =
        ImportOne(
          ctx.STRING.txt.slice(1, ctx.STRING.txt.length - 1),
          IdentBuilder(ctx.ident)
        ) withLoc ctx.loc.copy(point = ctx.STRING.loc.start)
    }

    Visitor(ctx)
  }

}
