////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

sealed trait FuncVariant

object FuncVariant {
  case object Ctrl extends FuncVariant
  case object Comb extends FuncVariant
  case object Xeno extends FuncVariant
  case object Static extends FuncVariant
  case object Method extends FuncVariant
}
