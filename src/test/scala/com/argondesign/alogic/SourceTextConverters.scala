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
// Utility to be used in tests to build trees out of some text
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.frontend.Parser
import com.argondesign.alogic.frontend.Parser.Parseable

object SourceTextConverters {

  case class AsTreeSyntaxErrorException(cc: CompilerContext) extends Exception

  implicit class String2Repr(val string: String) {

    val source: Source = Source("<asTree>", string)

    def asTree[T <: Tree: Parseable](implicit cc: CompilerContext): T =
      Parser[T](source, SourceContext.Unknown) getOrElse { throw AsTreeSyntaxErrorException(cc) }

    def asTree[T <: Tree: Parseable](sc: SourceContext)(implicit cc: CompilerContext): T =
      Parser[T](source, sc) getOrElse { throw AsTreeSyntaxErrorException(cc) }

  }

}
