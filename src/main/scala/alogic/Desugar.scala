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
    case Update(lhs, op, rhs) => Assign(lhs, BinaryOp(Bracket(lhs.toExpr), op.init, Bracket(rhs)))
    case Plusplus(lhs)        => Assign(lhs, BinaryOp(Bracket(lhs.toExpr), "+", Num(false, Some(1), 1)))
    case Minusminus(lhs)      => Assign(lhs, BinaryOp(Bracket(lhs.toExpr), "-", Num(false, Some(1), 1)))
  }
}
