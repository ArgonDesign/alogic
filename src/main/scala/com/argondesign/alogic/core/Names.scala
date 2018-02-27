////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Names are used to represent identifiers scoped to various namespaces
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

object Names {

  abstract sealed trait Name {
    val str: String
    override def toString = str
  }

  // Used for names that stand for terms
  case class TermName(str: String) extends Name
  // Used for names that stand for types
  case class TypeName(str: String) extends Name
}
