
package alogic
import AstOps._

object Desugar {
  def RemoveAssigns(tree: StateProgram): StateProgram = {
    val ast = RewriteAST(tree) {
      case Update(lhs, op, rhs) => Some(Assign(lhs, BinaryOp(Bracket(lhs), op.init, Bracket(rhs))))
      case Plusplus(lhs)        => Some(Assign(lhs, BinaryOp(Bracket(lhs), "+", Num("1'b1"))))
      case Minusminus(lhs)      => Some(Assign(lhs, BinaryOp(Bracket(lhs), "-", Num("1'b1"))))
      case _                    => None
    }
    ast.asInstanceOf[StateProgram]
  }
}
