////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build a Pkg AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object PkgBuilder extends BaseBuilder[PkgContext, Pkg] {

  def apply(ctx: PkgContext)(implicit mb: MessageBuffer, sc: SourceContext): Pkg = {
    object Visitor extends AlogicScalarVisitor[Pkg] {
      override def visitPkgDesc(ctx: PkgDescContext): Pkg =
        PkgSplice(DescBuilder(ctx.desc)(mb, SourceContext.Package)) withLoc ctx.loc

      override def visitPkgImport(ctx: PkgImportContext): Pkg =
        PkgSplice(ImportBuilder(ctx.imprt)) withLoc ctx.loc

      override def visitPkgUsing(ctx: PkgUsingContext): Pkg =
        PkgSplice(UsingBuilder(ctx.using)) withLoc ctx.loc

      override def visitPkgFrom(ctx: PkgFromContext): Pkg =
        PkgSplice(FromBuilder(ctx.from)) withLoc ctx.loc

      override def visitPkgAssertion(ctx: PkgAssertionContext): Pkg =
        PkgSplice(AssertionBuilder(ctx.assertion)) withLoc ctx.loc

      override def visitPkgCompile(ctx: PkgCompileContext): Pkg =
        PkgCompile(
          ExprBuilder(ctx.expr()),
          Option.when(ctx.ident != null)(IdentBuilder(ctx.ident))
        ) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
