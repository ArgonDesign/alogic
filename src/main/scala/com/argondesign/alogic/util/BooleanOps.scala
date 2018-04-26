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
// Provide 'bool option { a }', instead of 'if (bool) Some(a) else None'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import scala.language.implicitConversions

// For importing with BooleanOps._
object BooleanOps {
  implicit final class BooleanOpsImpl(val bool: Boolean) extends AnyVal {
    def option[T](v: => T): Option[T] = if (bool) Some(v) else None
  }
}

// For mixing into classes
trait BooleanOps {
  import BooleanOps.BooleanOpsImpl
  implicit final def boolean2BooleanOpsImpl(bool: Boolean): BooleanOpsImpl = {
    new BooleanOpsImpl(bool)
  }
}
