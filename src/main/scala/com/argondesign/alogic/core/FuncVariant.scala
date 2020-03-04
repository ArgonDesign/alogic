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

package com.argondesign.alogic.core

sealed trait FuncVariant

object FuncVariant {
  case object None extends FuncVariant
  case object Ctrl extends FuncVariant
  case object Comb extends FuncVariant
}
