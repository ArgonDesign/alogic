////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.Symbol

trait DefnOps { this: Defn =>

  def descs: List[Desc] = Nil

  def defns: List[Defn] = Nil

  final lazy val initializer: Option[Expr] = this match {
    case DefnVar(_, iOpt) => iOpt
    case DefnOut(_, iOpt) => iOpt
    case DefnConst(_, i)  => Some(i)
    case DefnGen(_, i)    => Some(i)
    case _                => None
  }

  def cpy(symbol: Symbol): Defn = this match {
    case node: DefnVar       => node.copy(symbol = symbol)
    case node: DefnIn        => node.copy(symbol = symbol)
    case node: DefnOut       => node.copy(symbol = symbol)
    case node: DefnPipeline  => node.copy(symbol = symbol)
    case node: DefnConst     => node.copy(symbol = symbol)
    case node: DefnGen       => node.copy(symbol = symbol)
    case node: DefnArray     => node.copy(symbol = symbol)
    case node: DefnSram      => node.copy(symbol = symbol)
    case node: DefnStack     => node.copy(symbol = symbol)
    case node: DefnType      => node.copy(symbol = symbol)
    case node: DefnEntity    => node.copy(symbol = symbol)
    case node: DefnRecord    => node.copy(symbol = symbol)
    case node: DefnInstance  => node.copy(symbol = symbol)
    case node: DefnSingleton => node.copy(symbol = symbol)
    case node: DefnFunc      => node.copy(symbol = symbol)
    case node: DefnState     => node.copy(symbol = symbol)
  }

}

trait DefnObjOps { self: Defn.type =>

  final def unapply(defn: Defn): Option[Symbol] = Some(defn.symbol)

}
