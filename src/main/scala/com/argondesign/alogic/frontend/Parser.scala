////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// The parser takes program source and returns the corresponding Tree.
// The parser itself is generated using Antlr4, this is just a small wrapper to
// invoke it on some source to build an Abstract Syntax Tree.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.antlr._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.SourceContext
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext

import scala.util.chaining._

object Parser {

  abstract class Parseable[T <: Tree] {
    type C <: ParserRuleContext
    def parse(parser: AlogicParser): C
    def build(ctx: C)(implicit mb: MessageBuffer, sc: SourceContext): T
  }

  // apply is polymorphic in its output. We achieve this through using an implicitly
  // provided dispatcher to dispatch to the appropriate parser entry points
  def apply[T <: Tree: Parseable](
      source: Source,
      sc: SourceContext,
      start: Int = 0,
      end: Int = -1
    )(
      implicit
      mb: MessageBuffer
    ): Option[T] = {
    require(start >= 0)
    require(end < 0 || end >= start)

    val dispatcher = implicitly[Parseable[T]]

    // Create Antlr4 parser
    val tokenFactory = new AlogicTokenFactory(source)
    val parser = {
      val text = if (end >= 0) source.text.take(end) else source.text
      val stream = CharStreams.fromString(text, source.name)
      stream.seek(start)
      val lexer = new AlogicLexer(stream)
      lexer.setTokenFactory(tokenFactory)
      new AlogicParser(new CommonTokenStream(lexer)) tap { parser =>
        parser.removeErrorListeners()
        parser.addErrorListener(new AlogicParseErrorListener)
      }
    }

    // Build Antlr4 parse tree
    val ctx = dispatcher.parse(parser)

    // Build the Abstract Syntax Tree from the Parse Tree (assuming no syntax)
    Option.when(parser.getNumberOfSyntaxErrors == 0 && !tokenFactory.hadError) {
      dispatcher.build(ctx)(mb, sc) tap {
        // $COVERAGE-OFF$ Debug code
        // Ensure all nodes have locations TODO: Make optional
        _ visitAll { case tree: Tree if !tree.hasLoc => throw Ice(s"Tree has no location $tree") }
        // $COVERAGE-ON$
      }
    }
  }

}
