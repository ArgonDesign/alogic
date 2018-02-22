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
// Denotations are used to attach information to symbols. While symbols
// are unique and never change. The Denotation attached to a symbol can change
// from one compiler phase to another as the tree is being transformed.
// Denotation instances are still immutable.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.Names.EntityName
import com.argondesign.alogic.core.Names.FuncName
import com.argondesign.alogic.core.Names.Name
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Names.TypeName

object Denotations {

  abstract sealed trait Denotation {
    val name: Name
  }

  case class TermDenotation(name: TermName) extends Denotation
  case class FuncDenotation(name: FuncName) extends Denotation
  case class TypeDenotation(name: TypeName) extends Denotation
  case class EntityDenotation(name: EntityName) extends Denotation

}
