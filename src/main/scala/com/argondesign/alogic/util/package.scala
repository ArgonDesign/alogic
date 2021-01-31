////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Very broadly used utility functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

package object util {
  // $COVERAGE-OFF$
  def unreachable: Nothing = throw UnreachableException()
  // $COVERAGE-ON$
}
