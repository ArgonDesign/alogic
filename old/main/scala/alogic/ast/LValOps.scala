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

package alogic.ast

trait LValOps { this: LVal =>

  def toExpr: Expr = this match {
    case LValName(a, names)                              => DottedName(a, names)
    case LValArrayLookup(a0, LValName(a1, names), index) => ExprArrIndex(a0, DottedName(a1, names), index)
    case LValSlice(a, ref, lidx, op, ridx)               => Slice(a, ref.toExpr, lidx, op, ridx)
    case LValCat(a, parts)                               => BitCat(a, parts map { _.toExpr })
  }
}
