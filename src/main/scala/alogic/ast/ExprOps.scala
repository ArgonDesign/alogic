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

import scala.collection.mutable
import scala.math.BigInt.int2bigInt

import alogic.Message
import alogic.Loc

trait ExprOps { this: Expr =>

  def *(rhs: Expr) = BinaryOp(this.attr, this, "*", rhs)
  def /(rhs: Expr) = BinaryOp(this.attr, this, "/", rhs)
  def %(rhs: Expr) = BinaryOp(this.attr, this, "%", rhs)
  def +(rhs: Expr) = BinaryOp(this.attr, this, "+", rhs)
  def -(rhs: Expr) = BinaryOp(this.attr, this, "-", rhs)
  def <<(rhs: Expr) = BinaryOp(this.attr, this, "<<", rhs)
  def >>(rhs: Expr) = BinaryOp(this.attr, this, ">>", rhs)
  def >>>(rhs: Expr) = BinaryOp(this.attr, this, ">>>", rhs)
  def &(rhs: Expr) = BinaryOp(this.attr, this, "&", rhs)
  def ^(rhs: Expr) = BinaryOp(this.attr, this, "^", rhs)
  def |(rhs: Expr) = BinaryOp(this.attr, this, "|", rhs)
  def &&(rhs: Expr) = BinaryOp(this.attr, this, "&&", rhs)
  def ||(rhs: Expr) = BinaryOp(this.attr, this, "||", rhs)

  def *(rhs: Int) = BinaryOp(this.attr, this, "*", Num(this.attr, true, None, rhs))
  def /(rhs: Int) = BinaryOp(this.attr, this, "/", Num(this.attr, true, None, rhs))
  def %(rhs: Int) = BinaryOp(this.attr, this, "%", Num(this.attr, true, None, rhs))
  def +(rhs: Int) = BinaryOp(this.attr, this, "+", Num(this.attr, true, None, rhs))
  def -(rhs: Int) = BinaryOp(this.attr, this, "-", Num(this.attr, true, None, rhs))
  def <<(rhs: Int) = BinaryOp(this.attr, this, "<<", Num(this.attr, true, None, rhs))
  def >>(rhs: Int) = BinaryOp(this.attr, this, ">>", Num(this.attr, true, None, rhs))
  def >>>(rhs: Int) = BinaryOp(this.attr, this, ">>>", Num(this.attr, true, None, rhs))

  // Test if the expression is semantically constant. It may still contain parameters, so
  // .isConst does not imply that .eval will yield a result.
  lazy val isConst: Boolean = this match {
    case DottedName(_, name :: Nil) => symtab(name) match {
      case _: DeclParam => true
      case _: DeclConst => true
      case _            => false
    }

    case _: Num                       => true
    case _: Literal                   => true

    case _: DottedName                => false
    case _: ExprArrIndex              => false
    case _: ExprVecIndex              => false // TODO: const if CONST/PARAM decl
    case _: CallExpr                  => false
    case _: DollarCall                => false // Could handle some defined ones like $clog2
    case _: ReadCall                  => false
    case _: PipelineRead              => false
    case _: PipelineWrite             => false
    case _: WaitCall                  => false
    case _: ValidCall                 => false
    case _: WriteCall                 => false
    case _: ErrorExpr                 => false

    case Zxt(_, numbits, expr)        => numbits.isConst && expr.isConst
    case Sxt(_, numbits, expr)        => numbits.isConst && expr.isConst
    case BinaryOp(_, lhs, _, rhs)     => lhs.isConst && rhs.isConst
    case UnaryOp(_, _, lhs)           => lhs.isConst
    case Bracket(_, content)          => content.isConst
    case TernaryOp(_, cond, lhs, rhs) => cond.isConst && lhs.isConst && rhs.isConst // Could loosen this
    case BitRep(_, count, value)      => count.isConst && value.isConst
    case BitCat(_, terms)             => terms forall (_.isConst)
    case Slice(_, ref, lidx, _, ridx) => ref.isConst && lidx.isConst && ridx.isConst
  }

  // Test if the numerical value of the expression can be computed now.
  // If .isKnown is true, then .eval will yield the value
  lazy val isKnown: Boolean = this match {
    case DottedName(_, name :: Nil)   => false // Could relax for CONSTs

    case _: Num                       => true
    case _: Literal                   => true

    case _: DottedName                => false
    case _: ExprArrIndex              => false
    case _: ExprVecIndex              => false // TODO: const if CONST/PARAM decl
    case _: CallExpr                  => false
    case _: DollarCall                => false // Could handle some defined ones like $clog2
    case _: ReadCall                  => false
    case _: PipelineRead              => false
    case _: PipelineWrite             => false
    case _: WaitCall                  => false
    case _: ValidCall                 => false
    case _: WriteCall                 => false
    case _: ErrorExpr                 => false

    case Zxt(_, numbits, expr)        => numbits.isKnown && expr.isKnown
    case Sxt(_, numbits, expr)        => numbits.isKnown && expr.isKnown
    case BinaryOp(_, lhs, _, rhs)     => lhs.isKnown && rhs.isKnown
    case UnaryOp(_, _, lhs)           => lhs.isKnown
    case Bracket(_, content)          => content.isKnown
    case TernaryOp(_, cond, lhs, rhs) => cond.isKnown && lhs.isKnown && rhs.isKnown // Could loosen this
    case BitRep(_, count, value)      => count.isKnown && value.isKnown
    case BitCat(_, terms)             => terms forall (_.isKnown)
    case Slice(_, ref, lidx, _, ridx) => ref.isKnown && lidx.isKnown && ridx.isKnown
  }

  def eval: BigInt = this.simplify match {
    case Num(_, _, _, value) => value
    case x                   => Message.ice(this, s"Don't know how to eval '${x.toSource}'")
  }

  def simplify: Expr = this match {
    case Bracket(_, content) => content.simplify

    case UnaryOp(a, op, rhs) => UnaryOp(a, op, rhs.simplify) match {
      case x @ UnaryOp(_, op, rhs @ Num(_, true, None, rv)) => op match {
        case "+" => rhs
        case "-" => Num(a, true, None, -rv)
        case _   => x
      }
      case x @ UnaryOp(_, op, rhs @ Num(_, false, None, rv)) => op match {
        case "+" => rhs
        case "-" => { Message.error(x, "Taking negative of unsigned constnat"); x }
        case _   => x
      }
      case x => x
    }

    case BinaryOp(a, lhs, op, rhs) => BinaryOp(a, lhs.simplify, op, rhs.simplify) match {
      case x @ BinaryOp(_, Num(_, true, None, lv), op, Num(_, true, None, rv)) => op match {
        case "+" => Num(a, true, None, lv + rv)
        case "-" => Num(a, true, None, lv - rv)
        case "*" => Num(a, true, None, lv * rv)
        case _   => x
      }
      case x @ BinaryOp(_, Num(_, _, None, lv), op, Num(_, _, None, rv)) => op match {
        case "+" => Num(a, false, None, lv + rv)
        case "-" => Num(a, false, None, lv - rv)
        case "*" => Num(a, false, None, lv * rv)
        case _   => x
      }
      case x => x
    }

    case TernaryOp(a, cond, lhs, rhs)        => TernaryOp(a, cond.simplify, lhs.simplify, rhs.simplify)

    case Zxt(a, numbits, expr)               => Zxt(a, numbits.simplify, expr.simplify)
    case Sxt(a, numbits, expr)               => Sxt(a, numbits.simplify, expr.simplify)
    case CallExpr(a, name, args)             => CallExpr(a, name, args map { _.simplify })
    case DollarCall(a, name, args)           => DollarCall(a, name, args map { _.simplify })
    case WriteCall(a, name, args)            => WriteCall(a, name, args map { _.simplify })
    case BitRep(a, count: Expr, value: Expr) => BitRep(a, count.simplify, value.simplify)
    case BitCat(a, terms)                    => BitCat(a, terms map { _.simplify })
    case Slice(a, ref, lidx, op, ridx)       => Slice(a, ref, lidx.simplify, op, ridx.simplify) // TODO: simplify ref

    case x: ReadCall                         => x
    case x: WaitCall                         => x
    case x: ValidCall                        => x
    case x: DottedName                       => x
    case x: ExprArrIndex                     => x // TODO: simplify
    case x: ExprVecIndex                     => x // TODO: simplify
    case x: Num                              => x
    case x: Literal                          => x
    case x: PipelineRead                     => x
    case x: PipelineWrite                    => x
    case x: ErrorExpr                        => x
  }

  // Compute a new expression representing the width of this expression.
  // Return None if it cannot be determined.
  lazy val widthExpr: Option[Expr] = {
    this match {
      case name: DottedName => {
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
                  Message.fatal(this, s"No field named '$n' in struct '$name'") // TODO: check earlier
                }
              }
              case _ => Message.fatal(this, s"Cannot find field '$n' in non-struct type '$kind'")
            }
          }

          name match {
            case DottedName(_, n :: Nil) => Some(symtab(n)) map { decl => decl.kind }
            case DottedName(_, n :: ns)  => Some(symtab(n)) map { decl => lookUpField(ns, decl.kind) }
          }
        }
        kind map { x: Type => x.widthExpr }
      }
      case ExprArrIndex(_, name, _)     => name.widthExpr
      case _: ExprVecIndex              => ??? // TODO: handle IntVType properly
      case ReadCall(_, name)            => name.widthExpr
      case Num(_, _, Some(width), _)    => Some(Expr(width))
      case Num(_, _, None, _)           => None
      case _: CallExpr                  => Some(Expr(0))
      case Zxt(_, numbits, _)           => Some(numbits)
      case Sxt(_, numbits, _)           => Some(numbits)
      case _: DollarCall                => None
      case _: PipelineRead              => None
      case _: PipelineWrite             => None
      case _: WaitCall                  => Some(Expr(0))
      case _: ValidCall                 => Some(Expr(1))
      case _: WriteCall                 => Some(Expr(0))
      case _: BinaryOp                  => None
      case UnaryOp(_, _, lhs)           => lhs.widthExpr
      case Bracket(_, content)          => content.widthExpr
      case _: TernaryOp                 => None
      case Slice(_, _, lidx, ":", ridx) => Some(lidx - ridx + 1) // TODO: assert lidx >= ridx
      case Slice(_, _, _, _, width)     => Some(width)
      case _: Literal                   => None
      case _: ErrorExpr                 => None
      case BitRep(_, count, value) => value.widthExpr match {
        case Some(w) => Some(count * w)
        case None    => Message.fatal(value, "Cannot compute width of bit repetion")
      }
      case BitCat(_, terms) => {
        val exprs = for (term <- terms) yield {
          term.widthExpr match {
            case Some(expr) => expr
            case None       => Message.fatal(term, "Cannot compute width of bit concatenation operand")
          }
        }
        Some(exprs.reduce(_ + _))
      }
    }
  }

  // Compute a new expression representing the MSB of this expression.
  // Return None if it cannot be determined.
  def msbExpr: Option[Expr] = {
    this match {
      case name @ DottedName(a, _) => name.widthExpr map { width =>
        Slice(a, name, width - 1, "+:", Expr(1))
      }
      case vref @ ExprArrIndex(a, name, _) => name.widthExpr map { width =>
        Slice(a, vref, width - 1, "+:", Expr(1))
      }
      case _: ExprVecIndex => ??? // TODO: handle IntVType properly
      case ReadCall(a, name) => name.widthExpr map { width =>
        Slice(a, name, width - 1, "+:", Expr(1))
      }

      case Num(a, _, Some(width), value)    => Some(Num(a, false, Some(1), value >> (width - BigInt(1)).toInt))
      case Num(_, _, None, _)               => None
      case _: CallExpr                      => None
      case Zxt(_, _, expr)                  => None // TODO: if (numbits == expr.widthExpr.eval) expr.msbExpr else 0
      case Sxt(_, _, expr)                  => expr.msbExpr
      case _: DollarCall                    => None
      case _: PipelineRead                  => None
      case _: PipelineWrite                 => None
      case _: WaitCall                      => None
      case _: ValidCall                     => None
      case _: WriteCall                     => None
      case _: BinaryOp                      => None
      case UnaryOp(_, _, lhs)               => None // TODO: could handle ~
      case Bracket(_, content)              => content.msbExpr
      case _: TernaryOp                     => None
      case Slice(a, ref, lidx, ":", _)      => Some(Slice(a, ref, lidx, "+:", Expr(1)))
      case Slice(a, ref, base, "+:", width) => Some(Slice(a, ref, base + width - 1, "+:", Expr(1)))
      case Slice(a, ref, base, "-:", _)     => Some(Slice(a, ref, base, "+:", Expr(1)))
      case _: Slice                         => unreachable
      case BitRep(_, _, value)              => value.msbExpr
      case BitCat(_, terms)                 => terms.head.msbExpr
      case _: Literal                       => None
      case _: ErrorExpr                     => None
    }
  }

  def hasSideEffect: Boolean = this match {
    case _: CallExpr                   => true
    case _: DollarCall                 => true
    case _: ReadCall                   => true
    case _: WriteCall                  => true
    case _: PipelineRead               => true
    case _: PipelineWrite              => true
    case _: WaitCall                   => true
    case _: ErrorExpr                  => true

    case _: DottedName                 => false
    case _: Num                        => false
    case _: ValidCall                  => false
    case _: Literal                    => false

    case ExprArrIndex(_, _, indices)   => indices exists { _.hasSideEffect }
    case ExprVecIndex(_, ref, indices) => ref.hasSideEffect || (indices exists { _.hasSideEffect })
    case Zxt(_, _, e)                  => e.hasSideEffect
    case Sxt(_, _, e)                  => e.hasSideEffect
    case BinaryOp(_, l, _, r)          => l.hasSideEffect || r.hasSideEffect
    case UnaryOp(_, _, e)              => e.hasSideEffect
    case Bracket(_, e)                 => e.hasSideEffect
    case TernaryOp(_, c, t, f)         => c.hasSideEffect || t.hasSideEffect || f.hasSideEffect
    case BitRep(_, _, e)               => e.hasSideEffect
    case BitCat(_, terms)              => terms exists { _.hasSideEffect }
    case Slice(_, _, l, _, r)          => l.hasSideEffect || r.hasSideEffect
  }

  def toVerilog: String = this match {
    case Num(_, false, None, value)        => s"'d${value}"
    case Num(_, true, None, value)         => value.toString
    case Num(_, false, Some(width), value) => if (width == 1) s"1'b${value}" else s"${width}'d${value}"
    case Num(_, true, Some(width), value)  => s"${width}'sd${value}"
    case _: CallExpr                       => ???
    case Zxt(_, numbits, expr)             => ???
    case Sxt(_, numbits, expr)             => ???
    case _: DollarCall                     => ???
    case _: ReadCall                       => ???
    case _: PipelineRead                   => ???
    case _: PipelineWrite                  => ???
    case _: WaitCall                       => ???
    case _: ValidCall                      => ???
    case _: WriteCall                      => ???
    case BinaryOp(_, lhs, op, rhs)         => s"(${lhs.toVerilog}) $op (${rhs.toVerilog})"
    case UnaryOp(_, _, lhs)                => ???
    case Bracket(_, content)               => s"(${content})"
    case TernaryOp(_, cond, lhs, rhs)      => ???
    case BitRep(_, count, value)           => ???
    case BitCat(_, terms)                  => ???
    case _: Slice                          => ???
    case DottedName(_, name :: Nil)        => name
    case x: DottedName                     => Message.ice(x, s"Cannot generate verilog for '$x'")
    case _: ExprArrIndex                   => ???
    case _: ExprVecIndex                   => ???
    case _: Literal                        => ???
    case _: ErrorExpr                      => "/*Error expression*/"
  }

  def kind: Option[Type] = this match {
    case _: Num           => ???
    case _: CallExpr      => ???
    case _: Zxt           => ???
    case _: Sxt           => ???
    case _: DollarCall    => ???
    case _: ReadCall      => ???
    case _: PipelineRead  => ???
    case _: PipelineWrite => ???
    case _: WaitCall      => ???
    case _: ValidCall     => ???
    case _: WriteCall     => ???
    case _: BinaryOp      => ???
    case _: UnaryOp       => ???
    case _: Bracket       => ???
    case _: TernaryOp     => ???
    case _: BitRep        => ???
    case _: BitCat        => ???
    case _: Slice         => ???
    case _: DottedName    => ???
    case _: ExprArrIndex  => ???
    case _: ExprVecIndex  => ???
    case _: Literal       => ???
    case _: ErrorExpr     => ???
  }
}

trait ExprObjOps {
  def apply(n: Int) = Num(Attr.empty, true, None, n)
  def apply(n: BigInt) = Num(Attr.empty, true, None, n)
}
