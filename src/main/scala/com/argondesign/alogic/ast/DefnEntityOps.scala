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

trait DefnEntityOps { this: DefnEntity =>

  final lazy val decls: List[Decl] = body collect { case EntSplice(decl: Decl) => decl }

  final override lazy val defns: List[Defn] = body collect { case EntSplice(defn: Defn) => defn }

  final lazy val entities: List[DefnEntity] = defns collect {
    case defn: DefnEntity => defn
  }

  final lazy val instances: List[DefnInstance] = defns collect {
    case defn: DefnInstance => defn
  }

  final lazy val functions: List[DefnFunc] = defns collect {
    case defn: DefnFunc => defn
  }

  final lazy val states: List[DefnState] = defns collect {
    case defn: DefnState => defn
  }

  final lazy val assigns: List[EntAssign] = body collect { case node: EntAssign => node }

  final lazy val combProcesses: List[EntCombProcess] = body collect {
    case node: EntCombProcess => node
  }

  final lazy val clockedProcesses: List[EntClockedProcess] = body collect {
    case node: EntClockedProcess => node
  }

  final lazy val verbatims: List[EntVerbatim] = body collect { case node: EntVerbatim => node }

  final lazy val clk: Option[Symbol] =
    body collectFirst { case EntSplice(Defn(symbol)) if symbol.attr.clk.isSet => symbol }

  final lazy val rst: Option[Symbol] =
    body collectFirst { case EntSplice(Defn(symbol)) if symbol.attr.rst.isSet => symbol }

  final lazy val go: Option[Symbol] =
    body collectFirst { case EntSplice(Defn(symbol)) if symbol.attr.go.isSet => symbol }
}
