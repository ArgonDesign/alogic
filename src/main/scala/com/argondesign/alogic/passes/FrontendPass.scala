////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees.Arg
import com.argondesign.alogic.ast.Trees.DescPackage
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.frontend.Frontend

object FrontendPass
    extends SimplePass[(Source, Loc, List[Arg]), Option[(DescPackage, Iterable[DescPackage])]] {
  val name = "frontend"

  override protected def dump(
      result: Option[(DescPackage, Iterable[DescPackage])],
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit = result foreach {
    case (input, dependencies) =>
      cc.dump(input, "." + tag)
      dependencies foreach { cc.dump(_, "." + tag) }
  }

  override protected def process(
      input: (Source, Loc, List[Arg])
    )(
      implicit
      cc: CompilerContext
    ): Option[(DescPackage, Iterable[DescPackage])] = {
    val (source, loc, params) = input
    val frontend = new Frontend
    frontend(source, loc, params)
  }

}
