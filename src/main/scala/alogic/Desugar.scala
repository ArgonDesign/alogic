
package alogic
import AstOps._

object Desugar {
  def RemoveAssigns(tree: StateProgram): StateProgram = {
    val ast = RewriteAST(tree) {
      case Assign(lhs, op, rhs) if (op.length > 1) => Some(Assign(lhs, "=", BinaryOp(Bracket(lhs), op.substring(0, op.length - 1), Bracket(rhs))))
      case Plusplus(lhs)                           => Some(Assign(lhs, "=", BinaryOp(Bracket(lhs), "+", Num("1'b1"))))
      case Minusminus(lhs)                         => Some(Assign(lhs, "=", BinaryOp(Bracket(lhs), "-", Num("1'b1"))))
      case _                                       => None
    }
    ast.asInstanceOf[StateProgram]
  }
}
