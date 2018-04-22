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

import com.argondesign.alogic.core.Names._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._

object Denotations {
  abstract sealed trait Denotation extends Product {
    val symbol: Symbol
    val name: Name
    val kind: Type
  }

  case class TermDenotation(
      symbol: TermSymbol,
      name: TermName,
      kind: Type,
  ) extends Denotation

  case class TypeDenotation(
      symbol: TypeSymbol,
      name: TypeName,
      kind: Type,
  ) extends Denotation
}
