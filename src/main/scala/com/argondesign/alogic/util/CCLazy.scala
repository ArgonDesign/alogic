////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
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

package com.argondesign.alogic.util

import com.argondesign.alogic.core.CompilerContext

final class CCLazy[T <: AnyRef](f: CompilerContext => T) {
  private var value: T = _

  def apply(cc: CompilerContext): T = {
    if (value == null) {
      value = f(cc)
    }
    value
  }
}

object CCLazy {
  def apply[T <: AnyRef](f: CompilerContext => T): CCLazy[T] = new CCLazy(f)
}
