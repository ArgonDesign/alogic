////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Very broadly used utility functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

package object util {
  def unreachable: Nothing = {
    import com.argondesign.alogic.core.ICE
    import com.argondesign.alogic.core.InternalCompilerErrorException
    println(ICE(Seq("unreachable")))
    throw new InternalCompilerErrorException(None)
  }
}
