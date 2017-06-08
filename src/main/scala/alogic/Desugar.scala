
package alogic
import AstOps._

object Desugar {
  def RemoveAssigns(tree: StateProgram): StateProgram = tree rewrite {
    case Update(lhs, op, rhs) => Assign(lhs, BinaryOp(Bracket(lhs), op.init, Bracket(rhs)))
    case Plusplus(lhs)        => Assign(lhs, BinaryOp(Bracket(lhs), "+", Num("1'b1")))
    case Minusminus(lhs)      => Assign(lhs, BinaryOp(Bracket(lhs), "-", Num("1'b1")))
  }
}
