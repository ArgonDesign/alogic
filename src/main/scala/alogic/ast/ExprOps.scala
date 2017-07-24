////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////


package alogic.ast

import alogic.Message

object ExprOps {
  implicit class Wrapper(val expr: Expr) extends AnyVal {

    // Test it the expression is univeraslly constant, i.e.: contains no unbound variables
    def isConst: Boolean = expr match {
      case _: Num                    => true
      case _: CallExpr               => false
      case Zxt(numbits, expr)        => numbits.isConst && expr.isConst
      case Sxt(numbits, expr)        => numbits.isConst && expr.isConst
      case _: DollarCall             => false // Could handle some defined ones like $clog2
      case _: ReadCall               => false
      case _: LockCall               => false
      case _: UnlockCall             => false
      case _: ValidCall              => false
      case _: WriteCall              => false
      case BinaryOp(lhs, _, rhs)     => lhs.isConst && rhs.isConst
      case UnaryOp(_, lhs)           => lhs.isConst
      case Bracket(content)          => content.isConst
      case TernaryOp(cond, lhs, rhs) => cond.isConst && lhs.isConst && rhs.isConst // Could loosen this
      case BitRep(count, value)      => count.isConst && value.isConst
      case BitCat(terms)             => terms forall (_.isConst)
      case _: Slice                  => false
      case _: DottedName             => false
      case _: ArrayLookup            => false
      case _: Literal                => true
    }

    def eval: BigInt = expr match {
      case Num(_, _, value)          => value
      case _: CallExpr               => Message.ice("unreachable")
      case Zxt(numbits, expr)        => expr.eval
      case Sxt(numbits, expr)        => expr.eval
      case _: DollarCall             => Message.ice("unreachable") // Could handle some defined ones like $clog2
      case _: ReadCall               => Message.ice("unreachable")
      case _: LockCall               => Message.ice("unreachable")
      case _: UnlockCall             => Message.ice("unreachable")
      case _: ValidCall              => Message.ice("unreachable")
      case _: WriteCall              => Message.ice("unreachable")
      case BinaryOp(lhs, "+", rhs)   => lhs.eval + rhs.eval
      case BinaryOp(lhs, "-", rhs)   => lhs.eval - rhs.eval
      case BinaryOp(lhs, "*", rhs)   => lhs.eval * rhs.eval
      case BinaryOp(lhs, _, rhs)     => ???
      case UnaryOp(_, lhs)           => ???
      case Bracket(content)          => content.eval
      case TernaryOp(cond, lhs, rhs) => ???
      case BitRep(count, value)      => ???
      case BitCat(terms)             => ???
      case _: Slice                  => Message.ice("unreachable")
      case _: DottedName             => Message.ice("unreachable")
      case _: ArrayLookup            => Message.ice("unreachable")
      case _: Literal                => Message.ice("unreachable")
    }

    def toVerilog: String = expr match {
      case Num(None, None, value)               => s"${value}"
      case Num(None, Some(width), value)        => Message.ice("unreachable")
      case Num(Some(false), None, value)        => s"'d${value}"
      case Num(Some(true), None, value)         => s"'sd${value}"
      case Num(Some(false), Some(width), value) => if (width == 1) s"1'b${value}" else s"${width}'d${value}"
      case Num(Some(true), Some(width), value)  => s"${width}'sd${value}"
      case _: CallExpr                          => ???
      case Zxt(numbits, expr)                   => ???
      case Sxt(numbits, expr)                   => ???
      case _: DollarCall                        => ???
      case _: ReadCall                          => ???
      case _: LockCall                          => ???
      case _: UnlockCall                        => ???
      case _: ValidCall                         => ???
      case _: WriteCall                         => ???
      case BinaryOp(lhs, op, rhs)               => s"(${lhs.toVerilog}) $op (${rhs.toVerilog})"
      case UnaryOp(_, lhs)                      => ???
      case Bracket(content)                     => ???
      case TernaryOp(cond, lhs, rhs)            => ???
      case BitRep(count, value)                 => ???
      case BitCat(terms)                        => ???
      case _: Slice                             => ???
      case DottedName(name :: Nil)              => name
      case x: DottedName                        => Message.ice(s"Cannot generate verilog for '$x'")
      case _: ArrayLookup                       => ???
      case _: Literal                           => ???
    }

    // Test if the expression is constant given the provided name bindings
    //def isConstWith(bindings) = ???
  }
}
