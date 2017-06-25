
package alogic.ast

import alogic.ast.ExprOps._
import alogic.Message

// Signals have a name, a width and a signedness
case class Signal(name: String, signed: Boolean, width: Expr) {
  def declString = {
    val signedStr = if (signed) "signed " else ""
    if (width.isConst) {
      val w = width.eval
      if (w < 1) {
        Message.fatal(s"Cannot declare signal with width ${w}")
      } else if (w == 1) {
        s"wire ${signedStr}${name};"
      } else {
        s"wire ${signedStr}[${w - 1}:0] ${name};"
      }
    } else {
      s"wire ${signedStr}[(${width.toVerilog})-1:0] ${name};"
    }
  }
}
