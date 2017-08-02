////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import alogic.Message

// Signals have a name, and a scalar type (which carries a width and a signedness)
case class Signal(name: String, kind: ScalarType) {
  def declString = {
    val signedStr = if (kind.signed) "signed " else ""
    val width = kind.widthExpr.simplify
    if (width.isConst) {
      val w = width.eval
      if (w < 1) {
        Message.fatal(s"Cannot declare signal with width ${w}")
      } else if (w == 1) {
        s"${signedStr}${name}"
      } else {
        s"${signedStr}[${w - 1}:0] ${name}"
      }
    } else {
      s"${signedStr}[${(width - 1).simplify.toVerilog}:0] ${name}"
    }
  }
}
