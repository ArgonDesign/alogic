////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@bits'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend

object AtBits extends BuiltinPolyFunc("@bits") {

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] =
    args.map(_.tpe) match {
      case List(TypeType(kind)) if kind.isPacked => Some(TypeNum(false))
      case List(TypeNone(kind)) if kind.isPacked => Some(TypeNum(false))
      case List(kind) if kind.isPacked           => Some(TypeNum(false))
      case _                                     => None
    }

  def isKnown(args: List[Expr]) = true

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]): Option[Expr] = args.head.tpeOpt map {
    case TypeType(kind) => Expr(kind.width)
    case TypeNone(kind) => Expr(kind.width)
    case kind           => Expr(kind.width)
  }

}
