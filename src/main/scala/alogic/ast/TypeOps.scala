////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

object TypeOps {
  implicit class TypeOpsWrapper(val kind: Type) extends AnyVal {
    def width: Expr = kind match {
      case IntType(_, size)       => Num(None, None, size)
      case IntVType(_, sizeExprs) => ???
      case Struct(_, fields) => if (fields.size == 1) {
        fields.values.head.width
      } else {
        val widths = fields.values map (_.width)
        widths reduce (BinaryOp(_, "+", _))
      }
    }
  }
}
