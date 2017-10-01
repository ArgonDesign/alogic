////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Container for ast.Node attributes
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import org.antlr.v4.runtime.ParserRuleContext

import alogic.Antlr4Conversions._
import alogic.Loc

class Attr(val loc: Loc, val symtab: String => Decl);

object Attr {
  def apply(loc: Loc, symtab: String => Decl) = new Attr(loc, symtab)

  def apply(ctx: ParserRuleContext)(implicit symtab: Option[Symtab]) = {
    val loc = ctx.loc
    val sfn: String => Decl = symtab.map(_.funcify(ctx)).getOrElse(_ => unreachable)
    new Attr(loc, sfn)
  }

  val empty = Attr(Loc("", 0), _ => unreachable)
}
