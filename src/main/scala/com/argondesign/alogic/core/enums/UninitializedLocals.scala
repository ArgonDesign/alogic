////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Representations of unintialzed local variabel handling methods
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core.enums

object UninitializedLocals extends Enumeration {
  type Type = Value
  val None = Value
  val Zeros = Value
  val Ones = Value
  val Random = Value
}
