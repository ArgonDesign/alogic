package alogic

import alogic.ast._
import alogic.ast.AstOps._

object Desugar {
  def RemoveAssigns(tree: AlogicTask): AlogicTask = tree rewrite {
    case Update(lhs, op, rhs) => Assign(lhs, BinaryOp(Bracket(lhs), op.init, Bracket(rhs)))
    case Plusplus(lhs)        => Assign(lhs, BinaryOp(Bracket(lhs), "+", Num("1'b1")))
    case Minusminus(lhs)      => Assign(lhs, BinaryOp(Bracket(lhs), "-", Num("1'b1")))
  }
}
