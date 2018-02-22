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
    val string: String
  }

  // Used for names that can participate in expressions e.g.: int i;
  case class TermName(string: String) extends Name
  // Used for functions e.g.: void main() { ... }
  case class FuncName(string: String) extends Name
  // Used for type names e.g.: struct foo {} / typedef u8 bar;
  case class TypeName(string: String) extends Name
  // Used for design entities e.g.: fsm alu { ... }
  case class EntityName(string: String) extends Name

}
