////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Common functionality of AST builders
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

import scala.jdk.CollectionConverters._

trait BaseBuilder[C, T] {

  def apply(ctx: C)(implicit mb: MessageBuffer, sc: SourceContext): T

  def apply(
      ctxs: java.util.List[_ <: C]
    )(
      implicit
      mb: MessageBuffer,
      sc: SourceContext
    ): List[T] = List from { ctxs.iterator.asScala map apply }

}
