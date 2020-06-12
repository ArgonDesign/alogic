////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

trait GenOps { this: Gen =>

  final lazy val mayGenerateParam: Boolean = {
    def check(trees: List[Tree]): Boolean = trees exists {
      case _: DescParam | _: DescParamType => true
      case gen: Gen                        => gen.mayGenerateParam
      case _                               => false
    }
    this match {
      case GenIf(_, thenItems, elseItems) => check(thenItems) || check(elseItems)
      case GenFor(_, _, _, body)          => check(body)
      case GenRange(_, _, _, body)        => check(body)
    }
  }

}
