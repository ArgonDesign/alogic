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
    case LValName(names)                         => DottedName(names)
    case LValArrayLookup(LValName(names), index) => ExprArrIndex(DottedName(names), index)
    case LValSlice(ref, lidx, op, ridx)          => Slice(ref.toExpr, lidx, op, ridx)
    case LValCat(parts)                          => BitCat(parts map { _.toExpr })
  }
}
