////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Error handler for Antlr4 generated parser
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.core.CompilerContext

import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

class ParserErrorListener(implicit cc: CompilerContext) extends BaseErrorListener {
  override def syntaxError(
    recognizer:         Recognizer[_, _],
    offendingSymbol:    Object,
    line:               Int,
    charPositionInLine: Int,
    msg:                String,
    e:                  RecognitionException
  ) = {
    val loc = cc.loc(recognizer.getInputStream.getSourceName, line)
    cc.error(loc, s"Syntax error: $msg")
  }
}
