////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogicr)

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
import com.argondesign.alogic.core.Loc
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

class ParseErrorListener(implicit cc: CompilerContext) extends BaseErrorListener {
  override def syntaxError(
      recogniser: Recognizer[_, _],
      offendingSymbol: Object,
      line: Int,
      charPositionInLine: Int,
      defaultMessage: String,
      e: RecognitionException
  ): Unit = {
    val source = recogniser.asInstanceOf[SourceMixin].source

    val offset = source.offsetFor(line) + charPositionInLine

    val loc = Loc(source, offset, offset, offset)

    cc.error(loc, s"Syntax error: $defaultMessage")
  }
}
