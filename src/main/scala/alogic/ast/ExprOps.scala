////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import alogic.Message
import alogic.ast.TypeOps._
import scala.collection.mutable

object ExprOps {
  implicit class ExprOpsWrapper(val expr: Expr) extends AnyVal {

    // Test it the expression is universally constant, i.e.: contains no unbound variables
    def isConst: Boolean = expr match {
      case _: Num                    => true
      case _: CallExpr               => false
      case Zxt(numbits, expr)        => numbits.isConst && expr.isConst
      case Sxt(numbits, expr)        => numbits.isConst && expr.isConst
      case _: DollarCall             => false // Could handle some defined ones like $clog2
      case _: ReadCall               => false
      case PipelineRead              => false
      case PipelineWrite             => false
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
      case PipelineRead              => Message.ice("unreachable")
      case PipelineWrite             => Message.ice("unreachable")
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
      case PipelineRead                         => ???
      case PipelineWrite                        => ???
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

    // Compute width of expression. Return None if it cannot be determined but the expression is legal.
    def width: Option[Expr] = expr.width(Map.empty[String, Declaration])
    def width(symtab: mutable.Map[String, Declaration]): Option[Expr] = expr.width(symtab.toMap)
    def width(symtab: Map[String, Declaration]): Option[Expr] = {
      def widthOfName(tree: DottedName): Option[Expr] = {
        val kind = {
          def lookUpField(names: List[String], kind: Type): Type = {
            val n :: ns = names
            kind match {
              case Struct(name, fields) => {
                if (fields contains n) {
                  ns match {
                    case Nil => fields(n)
                    case _   => lookUpField(ns, fields(n))
                  }
                } else {
                  Message.fatal(s"No field named '$n' in struct '$name'") // TODO: check earlier
                }
              }
              case _ => Message.fatal(s"Cannot find field '$n' in non-struct type '$kind'")
            }
          }

          tree match {
            case DottedName(n :: Nil) => symtab.get(n) map { decl => decl.decltype }
            case DottedName(n :: ns)  => symtab.get(n) map { decl => lookUpField(ns, decl.decltype) }
          }
        }

        kind map { x: Type => x.width }
      }

      expr match {
        case name: DottedName          => widthOfName(name)
        case ArrayLookup(name, _)      => widthOfName(name) // TODO: handle IntVType properly
        case ReadCall(name)            => widthOfName(name)

        case Num(_, None, _)           => None
        case Num(_, Some(width), _)    => Some(Num(None, None, width))
        case _: CallExpr               => Some(Num(None, None, 0))
        case Zxt(numbits, _)           => Some(numbits)
        case Sxt(numbits, _)           => Some(numbits)
        case _: DollarCall             => None
        case PipelineRead              => Some(Num(None, None, 0))
        case PipelineWrite             => Some(Num(None, None, 0))
        case _: LockCall               => Some(Num(None, None, 0))
        case _: UnlockCall             => Some(Num(None, None, 0))
        case _: ValidCall              => Some(Num(None, None, 1))
        case _: WriteCall              => Some(Num(None, None, 0))
        case _: BinaryOp               => ???
        case UnaryOp(_, lhs)           => lhs.width(symtab)
        case Bracket(content)          => content.width(symtab)
        case _: TernaryOp              => ???
        case Slice(_, lidx, ":", ridx) => Some(BinaryOp(BinaryOp(lidx, "-", ridx), "+", Num(None, None, 1))) // TODO: assert lidx >= ridx
        case Slice(_, _, _, width)     => Some(width)
        case _: Literal                => None
        case BitRep(count, value) => value.width(symtab) match {
          case Some(w) => Some(BinaryOp(count, "*", w))
          case None    => Message.fatal("Cannot compute width of bit repetion")
        }
        case BitCat(terms) => {
          val exprs = for (term <- terms) yield {
            term.width(symtab) match {
              case Some(expr) => expr
              case None       => Message.fatal("Cannot compute width of bit concatenation operand")
            }
          }
          Some(exprs.reduce(BinaryOp(_, "+", _)))
        }
      }
    }

    def hasSideEffect: Boolean = expr match {
      case _: CallExpr        => true
      case _: DollarCall      => true
      case _: ReadCall        => true
      case _: WriteCall       => true
      case PipelineRead       => true
      case PipelineWrite      => true
      case _: UnlockCall      => true
      case _: LockCall        => true

      case _: DottedName      => false
      case _: ArrayLookup     => false
      case _: Num             => false
      case _: ValidCall       => false

      case Zxt(_, e)          => e.hasSideEffect
      case Sxt(_, e)          => e.hasSideEffect
      case BinaryOp(l, _, r)  => l.hasSideEffect || r.hasSideEffect
      case UnaryOp(_, e)      => e.hasSideEffect
      case Bracket(e)         => e.hasSideEffect
      case TernaryOp(c, t, f) => c.hasSideEffect || t.hasSideEffect || f.hasSideEffect
      case BitRep(_, e)       => e.hasSideEffect
      case BitCat(terms)      => terms exists { _.hasSideEffect }
      case Slice(_, l, _, r)  => l.hasSideEffect || r.hasSideEffect
    }

    // Test if the expression is constant given the provided name bindings
    //def isConstWith(bindings) = ???
  }
}
