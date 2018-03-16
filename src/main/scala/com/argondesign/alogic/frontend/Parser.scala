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
// The parser takes program source and returns the coresponding Tree.
// The parser itself is generated using Antlr4, this is just a small wrapper to
// invoke it on some source to build an Abstract Syntax Tree.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.antlr._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext

object Parser {

  abstract class Parseable[T <: Tree] {
    type C <: ParserRuleContext
    def parse(parser: AlogicParser): C
    def build(ctx: C)(implicit cc: CompilerContext): T
  }

  // apply is polymorphic in its output. We achieve this through using an implicitly
  // provided dispatchers to dispatch to the appropriate parser entry points
  def apply[T <: Tree: Parseable](source: Source)(implicit cc: CompilerContext): Option[T] = {
    // Build Antlr4 parse tree
    val inputStream = CharStreams.fromString(source.text, source.name)

    val src = source

    val lexer = new AlogicLexer(inputStream) with SourceMixin { val source: Source = src }
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new AlogicParser(tokenStream) with SourceMixin { val source: Source = src }
    parser.removeErrorListeners()
    parser.addErrorListener(new AlogicParseErrorListener)

    val dispatcher = implicitly[Parseable[T]]

    val ctx = dispatcher.parse(parser)

    val opt = if (parser.getNumberOfSyntaxErrors != 0) None else Some(ctx)

    opt map { ctx =>
      // Build the Abstract Sytnax Tree from the Parse Tree
      val tree = dispatcher.build(ctx)

      // TODO: Make optional
      // Ensure all nodes have locations
      tree visitAll {
        case tree: Tree if !tree.hasLoc => cc.ice(s"Tree has no location $tree")
      }

      tree
    }
  }
}
