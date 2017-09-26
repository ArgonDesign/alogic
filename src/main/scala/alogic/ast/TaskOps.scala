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

trait TaskOps { this: Task =>
  val name: String
  val decls: List[Decl]

  lazy val defaultParams: Map[String, Expr] = {
    decls collect {
      case DeclParam(_, id, expr) => id -> expr
    }
  }.toMap
}
