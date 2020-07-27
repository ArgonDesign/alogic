////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

trait AssertionOps { this: Assertion =>

  def cpy(cond: Expr): Assertion = this match {
    // $COVERAGE-OFF$ Trivial to keep full, but not necessarily used
    case node: AssertionAssert => node.copy(cond = cond)
    case node: AssertionAssume => node.copy(cond = cond)
    case node: AssertionStatic => node.copy(cond = cond)
    // $COVERAGE-ON$
  }

}
