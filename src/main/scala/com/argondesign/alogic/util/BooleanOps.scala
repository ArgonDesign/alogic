////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Provide extension methods for Boolean
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

object BooleanOps {

  implicit final class BooleanClassOps(private val value: Boolean) extends AnyVal {
    // Implication
    def |->(other: => Boolean): Boolean = !value || other
  }

  implicit final class BooleanObjectOps(private val value: Boolean.type) extends AnyVal {
    //
  }

}
