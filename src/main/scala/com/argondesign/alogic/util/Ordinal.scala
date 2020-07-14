////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Cardinal Int to Ordinal String
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

object Ordinal {

  def apply(cardinal: Int): String = {
    require(cardinal > 0)
    cardinal % 100 match {
      case 11 | 12 | 13 => s"${cardinal}th"
      case rem =>
        rem % 10 match {
          case 1 => s"${cardinal}st"
          case 2 => s"${cardinal}nd"
          case 3 => s"${cardinal}rd"
          case _ => s"${cardinal}th"
        }
    }
  }

}
