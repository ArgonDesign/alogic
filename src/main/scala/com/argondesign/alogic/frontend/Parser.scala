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
// The parser takes program source and returns a parse tree.
// The parser itself is generated using Antlr4, this is just a small wrapper to
// invoke it on some source to build a parse tree.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.antlr.AlogicLexer
import com.argondesign.alogic.antlr.AlogicParseErrorListener
import com.argondesign.alogic.antlr.AlogicParser
import com.argondesign.alogic.antlr.AlogicParser.BlockContext
import com.argondesign.alogic.antlr.AlogicParser.DeclContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContext
import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AlogicParser.KindContext
import com.argondesign.alogic.antlr.AlogicParser.StartContext
import com.argondesign.alogic.antlr.AlogicParser.StatementContext
import com.argondesign.alogic.antlr.AlogicParser.Type_definitionContext
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext

object Parser {

  abstract class Dispatcher[T <: ParserRuleContext] {
    def dispatch(parser: AlogicParser): T
  }

  implicit val dispatcherStartContext = new Dispatcher[StartContext] {
    def dispatch(parser: AlogicParser): StartContext = parser.start()
  }

  implicit val dispatcherType_definitionContext = new Dispatcher[Type_definitionContext] {
    def dispatch(parser: AlogicParser): Type_definitionContext = parser.type_definition()
  }

  implicit val dispatcherKindContext = new Dispatcher[KindContext] {
    def dispatch(parser: AlogicParser): KindContext = parser.kind()
  }

  implicit val dispatcherEntityContext = new Dispatcher[EntityContext] {
    def dispatch(parser: AlogicParser): EntityContext = parser.entity()
  }

  implicit val dispatcherDeclContext = new Dispatcher[DeclContext] {
    def dispatch(parser: AlogicParser): DeclContext = parser.decl()
  }

  implicit val dispatcherConnectContext = new Dispatcher[ConnectContext] {
    def dispatch(parser: AlogicParser): ConnectContext = parser.connect()
  }

  implicit val dispatcherBlockContext = new Dispatcher[BlockContext] {
    def dispatch(parser: AlogicParser): BlockContext = parser.block()
  }

  implicit val dispatcherStatementContext = new Dispatcher[StatementContext] {
    def dispatch(parser: AlogicParser): StatementContext = parser.statement()
  }

  implicit val dispatcherExprContext = new Dispatcher[ExprContext] {
    def dispatch(parser: AlogicParser): ExprContext = parser.expr()
  }

  // apply is polymorphic in its output. We achieve this through using one of the
  // implicit dispatchers to dispatch to the appropriate parser entry point
  def apply[T <: ParserRuleContext](
    source: Source
  )(implicit
    cc: CompilerContext,
    dispatcher: Dispatcher[T]
  ): Option[T] = {
    val inputStream = CharStreams.fromString(source.text, source.name)

    val lexer = new AlogicLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new AlogicParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(new AlogicParseErrorListener)

    val parseTree = dispatcher.dispatch(parser)

    if (parser.getNumberOfSyntaxErrors != 0) None else Some(parseTree)
  }
}
