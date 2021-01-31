////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

trait CaseOps { this: Case =>

  def stmts: List[Stmt] = unreachable

  def cpy(stmts: List[Stmt]): Case = this match {
    case node: CaseRegular => node.copy(stmts = stmts)
    case node: CaseDefault => node.copy(stmts = stmts)
    case _: CaseSplice     => unreachable
  }

}
