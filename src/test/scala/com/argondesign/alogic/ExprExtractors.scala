////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Extractors for Expr nodes to be used in testing
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.builtins.AtUnknownI
import com.argondesign.alogic.builtins.AtUnknownU
import com.argondesign.alogic.builtins.DollarSigned
import com.argondesign.alogic.builtins.DollarUnsigned

object ExprExtractors {

  // Extractor for @unknownu(_: Int)
  object UnknownU {

    def unapply(expr: Expr): Option[Int] = expr match {
      case ExprBuiltin(AtUnknownU, ArgP(Expr(n)) :: Nil) => Some(n)
      case _                                             => None
    }

  }

  // Extractor for @unknowni(_: Int)
  object UnknownI {

    def unapply(expr: Expr): Option[Int] = expr match {
      case ExprBuiltin(AtUnknownI, ArgP(Expr(n)) :: Nil) => Some(n)
      case _                                             => None
    }

  }

  // $unsigned(_: Expr)
  object Unsigned {

    def unapply(expr: Expr): Option[Expr] = expr match {
      case ExprBuiltin(DollarUnsigned, ArgP(arg) :: Nil) => Some(arg)
      case _                                             => None
    }

  }

  // $signed(_: Expr)
  object Signed {

    def unapply(expr: Expr): Option[Expr] = expr match {
      case ExprBuiltin(DollarSigned, ArgP(arg) :: Nil) => Some(arg)
      case _                                           => None
    }

  }

}
