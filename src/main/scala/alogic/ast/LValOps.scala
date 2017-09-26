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

trait LValOps { this: LVal =>

  def toExpr: Expr = this match {
    case LValName(names)                         => DottedName(names)
    case LValArrayLookup(LValName(names), index) => ArrayLookup(DottedName(names), index)
    case LValSlice(ref, lidx, op, ridx)          => Slice(ref.toExpr, lidx, op, ridx)
    case LValCat(parts)                          => BitCat(parts map { _.toExpr })
  }

  //  def toVerilog: String = this match {
  //    case Num(false, None, value)        => s"'d${value}"
  //    case Num(true, None, value)         => value.toString
  //    case Num(false, Some(width), value) => if (width == 1) s"1'b${value}" else s"${width}'d${value}"
  //    case Num(true, Some(width), value)  => s"${width}'sd${value}"
  //    case _: CallExpr                    => ???
  //    case Zxt(numbits, expr)             => ???
  //    case Sxt(numbits, expr)             => ???
  //    case _: DollarCall                  => ???
  //    case _: ReadCall                    => ???
  //    case PipelineRead                   => ???
  //    case PipelineWrite                  => ???
  //    case _: WaitCall                    => ???
  //    case _: ValidCall                   => ???
  //    case _: WriteCall                   => ???
  //    case BinaryOp(lhs, op, rhs)         => s"(${lhs.toVerilog}) $op (${rhs.toVerilog})"
  //    case UnaryOp(_, lhs)                => ???
  //    case Bracket(content)               => s"(${content})"
  //    case TernaryOp(cond, lhs, rhs)      => ???
  //    case BitRep(count, value)           => ???
  //    case BitCat(terms)                  => ???
  //    case _: Slice                       => ???
  //    case DottedName(name :: Nil)        => name
  //    case x: DottedName                  => Message.ice(s"Cannot generate verilog for '$x'")
  //    case _: ArrayLookup                 => ???
  //    case _: Literal                     => ???
  //    case ErrorExpr                      => "/*Error expression*/"
  //  }
}
