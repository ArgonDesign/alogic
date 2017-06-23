
package alogic.ast

import alogic.ast.ExprOps._

// Signals have a name, a width and a signedness
case class Signal(name: String, signed: Boolean, width: Expr) {
  def declString = this match {
    case Signal(name, false, width) => s"wire [(${width.toVerilog})-1:0] ${name};"
    case Signal(name, true, width)  => s"wire signed [(${width.toVerilog})-1:0] ${name};"
  }
}
