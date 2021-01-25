////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Extractors for Expr nodes to be used in testing
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.Symbol

import scala.annotation.nowarn

object ExprExtractors {

  // Extractor for ExprCall to symbol with name with positional Int arguments
  object Call {

    def unapplySeq(expr: Expr): Option[(String, Seq[Int])] = expr match {
      case ExprCall(ExprSym(symbol), args) =>
        val argValues = args map {
          case ArgP(Expr(v)) => Some(v)
          case _             => None
        }
        Option.when(argValues.forall(_.nonEmpty)) {
          (symbol.name, argValues.flatten)
        }
      case _ => None
    }

  }

  // Extractor for @unknownu(_: Int)
  object UnknownU {

    @nowarn("msg=extracted sequence is not matched by a sequence wildcard")
    def unapply(expr: Expr): Option[Int] = expr match {
      case Call("@unknownu", n) => Some(n)
      case _                    => None
    }

  }

  // Extractor for @unknowni(_: Int)
  object UnknownI {

    @nowarn("msg=extracted sequence is not matched by a sequence wildcard")
    def unapply(expr: Expr): Option[Int] = expr match {
      case Call("@unknowni", n) => Some(n)
      case _                    => None
    }

  }

  // $unsigned(_: Expr)
  object Unsigned {

    def unapply(expr: Expr): Option[Expr] = expr match {
      case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(expr) :: Nil) => Some(expr)
      case _                                                         => None
    }

  }

}
