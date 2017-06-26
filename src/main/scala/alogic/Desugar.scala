
package alogic
import AstOps._

object Desugar {
  def RemoveAssigns(tree: Program): Program = {
    val ast = RewriteAST(tree) {
      case Assign(lhs, op, rhs) if (op.length == 2) => Some(Assign(lhs, "=", BinaryOp(Bracket(lhs), op.substring(0, 1), Bracket(rhs))))
      case Plusplus(lhs)                            => Some(Assign(lhs, "=", BinaryOp(Bracket(lhs), "+", Num("1'b1"))))
      case Minusminus(lhs)                          => Some(Assign(lhs, "=", BinaryOp(Bracket(lhs), "-", Num("1'b1"))))
      case _                                        => None
    }
    ast.asInstanceOf[Program]
  }
}
