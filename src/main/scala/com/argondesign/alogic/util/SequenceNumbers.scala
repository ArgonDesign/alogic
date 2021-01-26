////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Generates sequence numbers in a thread-safe fashion.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

class SequenceNumbers {
  private var n = -1

  def next: Int = synchronized {
    n += 1
    n
  }

}
