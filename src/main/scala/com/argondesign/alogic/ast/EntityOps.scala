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
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.util.unreachable

trait EntityOps { this: Entity =>
  lazy val declarations = this.body collect { case EntDecl(decl: Decl) => decl }

  lazy val entities = this.body collect { case EntEntity(entity) => entity }

  lazy val instances = this.body collect { case node: EntInstance => node }

  lazy val connects = this.body collect { case node: EntConnect => node }

  lazy val functions = this.body collect { case node: EntFunction => node }

  lazy val states = this.body collect { case node: EntState => node }

  lazy val combProcesses = this.body collect { case node: EntCombProcess => node }

  lazy val verbatims = this.body collect { case node: EntVerbatim => node }

  lazy val symbol = this.ref match {
    case Sym(symbol: TypeSymbol) => symbol
    case _                       => unreachable
  }

  lazy val name = this.ref match {
    case Ident(name) => name
    case Sym(symbol) => symbol.name
  }
}
