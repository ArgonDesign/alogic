////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Representations of entity variants
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core.enums

object EntityVariant extends Enumeration {
  type Type = Value
  val Fsm, Net, Ver = Value
}
