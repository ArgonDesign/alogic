////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
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

  class AsTreeSyntaxErrorException extends Exception

  implicit class String2Repr(val string: String) {

    val source: Source = Source("<asTree>", string)

    def asTree[T <: Tree: Parseable](
        sc: SourceContext = SourceContext.Unknown
      )(
        implicit
        cc: CompilerContext
      ): T =
      Parser[T](source, sc, cc.messageBuffer) getOrElse {
        throw new AsTreeSyntaxErrorException()
      }

  }

}
