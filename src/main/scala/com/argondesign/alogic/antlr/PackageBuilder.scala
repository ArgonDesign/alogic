////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build an DescPackage AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.FileContext
import com.argondesign.alogic.ast.Trees.DescPackage
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object PackageBuilder extends BaseBuilder[FileContext, DescPackage] {

  def apply(
      name: String,
      ctx: FileContext
    )(
      implicit
      mb: MessageBuffer,
      sc: SourceContext
    ): DescPackage = {
    val ref = Ident(name, Nil) withLoc ctx.loc
    DescPackage(ref, Nil, PkgBuilder(ctx.pkg)) withLoc ctx.loc.copy(
      line = 1,
      start = 0,
      point = 0,
      end = 0
    )
  }

  def apply(ctx: FileContext)(implicit mb: MessageBuffer, sc: SourceContext): DescPackage =
    PackageBuilder(ctx.loc.file, ctx)

}
