////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import alogic.ast._
import alogic.ast.AstOps._

object Desugar {
  def RemoveAssigns(tree: Task): Task = tree rewrite {
    case Update(a, lhs, op, rhs) => Assign(a, lhs, BinaryOp(a, Bracket(a, lhs.toExpr), op.init, Bracket(a, rhs)))
    case Plusplus(a, lhs)        => Assign(a, lhs, BinaryOp(a, Bracket(a, lhs.toExpr), "+", Num(a, false, Some(1), 1)))
    case Minusminus(a, lhs)      => Assign(a, lhs, BinaryOp(a, Bracket(a, lhs.toExpr), "-", Num(a, false, Some(1), 1)))
  }
}
