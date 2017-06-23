
package alogic.ast

object TypeOps {
  implicit class Wrapper(val kind: Type) extends AnyVal {
    def width: Expr = kind match {
      case IntType(_, size)       => Num(None, None, size)
      case IntVType(_, sizeExprs) => ???
      case Struct(fields) => if (fields.size == 1) {
        fields.values.head.width
      } else {
        val widths = fields.values map (_.width)
        widths reduce (BinaryOp(_, "+", _))
      }

      case State => ???
    }
  }
}
