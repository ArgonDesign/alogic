////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Representations of reset styles
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core.enums

object ResetStyle extends Enumeration {
  type Type = Value
  val AsyncLow, AsyncHigh, SyncLow, SyncHigh = Value
}
