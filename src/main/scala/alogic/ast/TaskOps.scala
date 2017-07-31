////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

trait TaskOps { this: Task =>
  val name: String
  val decls: List[Declaration]

  lazy val defaultParams: Map[String, Expr] = {
    decls collect {
      case ParamDeclaration(_, id, expr) => id -> expr
    }
  }.toMap
}
