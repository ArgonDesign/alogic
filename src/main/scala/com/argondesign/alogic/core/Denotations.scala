////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Denotations are used to associate information to symbols. While symbols
// are unique and never change. The Denotation associated to a symbol can change
// from one compiler phase to another as the tree is being transformed.
// Denotation instances are still immutable.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import Names.Name
import Names.TermName
import Names.TypeName
import Symbols.Symbol
import Symbols.TermSymbol
import Symbols.TypeSymbol
import Types._
import com.argondesign.alogic.ast.Trees.Expr

object Denotations {
  abstract sealed trait Denotation extends Product {
    val symbol: Symbol
    val name: Name
    val kind: Type
    val attr: Map[String, Expr]
  }

  case class TermDenotation(
      symbol: TermSymbol,
      name: TermName,
      kind: Type,
      attr: Map[String, Expr]
  ) extends Denotation

  case class TypeDenotation(
      symbol: TypeSymbol,
      name: TypeName,
      kind: Type,
      attr: Map[String, Expr]
  ) extends Denotation
}
