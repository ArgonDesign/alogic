////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import alogic.ast._
import alogic.ast.AstOps._

object Desugar {
  def RemoveAssigns(tree: Task): Task = tree rewrite {
    case Update(lhs, op, rhs) => Assign(lhs, BinaryOp(Bracket(lhs), op.init, Bracket(rhs)))
    case Plusplus(lhs)        => Assign(lhs, BinaryOp(Bracket(lhs), "+", Num(Some(false), Some(1), 1)))
    case Minusminus(lhs)      => Assign(lhs, BinaryOp(Bracket(lhs), "-", Num(Some(false), Some(1), 1)))
  }
}
