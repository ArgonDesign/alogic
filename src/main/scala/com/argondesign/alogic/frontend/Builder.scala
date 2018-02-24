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
// The builder takes a parse tree and returns the corresponding AST.
// The parser itself is implemented using Antlr4, visitors this is just a small
// wrapper to invoke it on some source to build a parse tree.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.antlr.AlogicParser.StartContext
import com.argondesign.alogic.antlr.RootBuilder
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable

import org.antlr.v4.runtime.ParserRuleContext

object Builder {
  def apply(ctx: ParserRuleContext)(implicit cc: CompilerContext): Tree = {
    val tree = ctx match {
      case ctx: StartContext => RootBuilder(ctx)
      case _                 => unreachable
    }

    // TODO: Make optional
    // Ensure all nodes have locations
    tree visitAll {
      case tree: Tree if !tree.hasLoc => cc.ice(s"Tree has no location $tree")
    }

    tree
  }
}
