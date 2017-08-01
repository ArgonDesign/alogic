////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

trait TypeOps extends TypePrettyPrintOps { this: Type =>
  def widthExpr: Expr = this match {
    case IntType(_, size)              => Num(None, None, size)
    case IntVType(_, sizeExprs :: Nil) => sizeExprs
    case IntVType(_, _)                => ???
    case Struct(_, fields) => if (fields.size == 1) {
      fields.values.head.widthExpr
    } else {
      val widths = fields.values map (_.widthExpr)
      widths reduce (BinaryOp(_, "+", _))
    }
  }
}
