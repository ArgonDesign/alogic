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
// Build a Map[String, Expr] from an attr Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

import scala.collection.JavaConverters._

object AttrBuilder extends BaseBuilder[AttrContext, Map[String, Expr]] {

  def apply(ctx: AttrContext)(implicit cc: CompilerContext): Map[String, Expr] = {
    val pairs = for (attrSpec <- ctx.attrspec.asScala.toList) yield {
      val key = attrSpec.IDENTIFIER.text
      val value = Option(attrSpec.expr) map { ExprBuilder(_) } getOrElse (Expr(1) withLoc ctx.loc)
      key -> value
    }

    Map(pairs: _*)
  }
}
