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

trait ExprOps { this: Expr =>

  def *(rhs: Expr) = BinaryOp(this, "*", rhs)
  def /(rhs: Expr) = BinaryOp(this, "/", rhs)
  def %(rhs: Expr) = BinaryOp(this, "%", rhs)
  def +(rhs: Expr) = BinaryOp(this, "+", rhs)
  def -(rhs: Expr) = BinaryOp(this, "-", rhs)
  def <<(rhs: Expr) = BinaryOp(this, "<<", rhs)
  def >>(rhs: Expr) = BinaryOp(this, ">>", rhs)
  def >>>(rhs: Expr) = BinaryOp(this, ">>>", rhs)
  def &(rhs: Expr) = BinaryOp(this, "&", rhs)
  def ^(rhs: Expr) = BinaryOp(this, "^", rhs)
  def |(rhs: Expr) = BinaryOp(this, "|", rhs)
  def &&(rhs: Expr) = BinaryOp(this, "&&", rhs)
  def ||(rhs: Expr) = BinaryOp(this, "||", rhs)

  def *(rhs: Int) = BinaryOp(this, "*", Num(true, None, rhs))
  def /(rhs: Int) = BinaryOp(this, "/", Num(true, None, rhs))
  def %(rhs: Int) = BinaryOp(this, "%", Num(true, None, rhs))
  def +(rhs: Int) = BinaryOp(this, "+", Num(true, None, rhs))
  def -(rhs: Int) = BinaryOp(this, "-", Num(true, None, rhs))
  def <<(rhs: Int) = BinaryOp(this, "<<", Num(true, None, rhs))
  def >>(rhs: Int) = BinaryOp(this, ">>", Num(true, None, rhs))
  def >>>(rhs: Int) = BinaryOp(this, ">>>", Num(true, None, rhs))

  // Test it the expression is universally constant, i.e.: contains no unbound variables
  lazy val isConst: Boolean = isConst(Map.empty[String, Decl])
  def isConst(symtab: scala.collection.Map[String, Decl]): Boolean = this match {
    case DottedName(name :: Nil) => symtab get name match {
      case Some(_: DeclParam) => true
      case Some(_: DeclConst) => true
      case _                  => false
    }

    case _: Num                    => true
    case _: Literal                => true

    case _: DottedName             => false
    case _: ExprArrIndex           => false
    case _: ExprVecIndex           => false // TODO: const if CONST/PARAM decl
    case _: CallExpr               => false
    case _: DollarCall             => false // Could handle some defined ones like $clog2
    case _: ReadCall               => false
    case PipelineRead              => false
    case PipelineWrite             => false
    case _: WaitCall               => false
    case _: ValidCall              => false
    case _: WriteCall              => false
    case ErrorExpr                 => false

    case Zxt(numbits, expr)        => numbits.isConst(symtab) && expr.isConst(symtab)
    case Sxt(numbits, expr)        => numbits.isConst(symtab) && expr.isConst(symtab)
    case BinaryOp(lhs, _, rhs)     => lhs.isConst(symtab) && rhs.isConst(symtab)
    case UnaryOp(_, lhs)           => lhs.isConst(symtab)
    case Bracket(content)          => content.isConst(symtab)
    case TernaryOp(cond, lhs, rhs) => cond.isConst(symtab) && lhs.isConst(symtab) && rhs.isConst(symtab) // Could loosen this
    case BitRep(count, value)      => count.isConst(symtab) && value.isConst(symtab)
    case BitCat(terms)             => terms forall (_.isConst(symtab))
    case Slice(ref, lidx, _, ridx) => ref.isConst(symtab) && lidx.isConst(symtab) && ridx.isConst(symtab)
  }

  def eval: BigInt = this.simplify match {
    case Num(_, _, value) => value
    case x                => Message.ice(s"Don't know how to eval '${x.toSource}'")
  }

  def simplify: Expr = this match {
    case Bracket(content) => content.simplify

    case UnaryOp(op, lhs) => UnaryOp(op, lhs.simplify)

    case BinaryOp(lhs, op, rhs) => BinaryOp(lhs.simplify, op, rhs.simplify) match {
      case x @ BinaryOp(Num(true, None, lv), op, Num(true, None, rv)) => op match {
        case "+" => Num(true, None, lv + rv)
        case "-" => Num(true, None, lv - rv)
        case "*" => Num(true, None, lv * rv)
        case _   => x
      }
      case x @ BinaryOp(Num(_, None, lv), op, Num(_, None, rv)) => op match {
        case "+" => Num(false, None, lv + rv)
        case "-" => Num(false, None, lv - rv)
        case "*" => Num(false, None, lv * rv)
        case _   => x
      }
      case x => x
    }

    case TernaryOp(cond, lhs, rhs)        => TernaryOp(cond.simplify, lhs.simplify, rhs.simplify)

    case Zxt(numbits, expr)               => Zxt(numbits.simplify, expr.simplify)
    case Sxt(numbits, expr)               => Sxt(numbits.simplify, expr.simplify)
    case CallExpr(name, args)             => CallExpr(name, args map { _.simplify })
    case DollarCall(name, args)           => DollarCall(name, args map { _.simplify })
    case WriteCall(name, args)            => WriteCall(name, args map { _.simplify })
    case BitRep(count: Expr, value: Expr) => BitRep(count.simplify, value.simplify)
    case BitCat(terms)                    => BitCat(terms map { _.simplify })
    case Slice(ref, lidx, op, ridx)       => Slice(ref, lidx.simplify, op, ridx.simplify) // TODO: simplify ref

    case x: ReadCall                      => x
    case x: WaitCall                      => x
    case x: ValidCall                     => x
    case x: DottedName                    => x
    case x: ExprArrIndex                  => x // TODO: simplify
    case x: ExprVecIndex                  => x // TODO: simplify
    case x: Num                           => x
    case x: Literal                       => x
    case PipelineRead                     => PipelineRead
    case PipelineWrite                    => PipelineWrite
    case ErrorExpr                        => ErrorExpr
  }

  private[this] def widthOfName(symtab: scala.collection.Map[String, Decl], tree: DottedName): Option[Expr] = {
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
        case DottedName(n :: Nil) => symtab.get(n) map { decl => decl.kind }
        case DottedName(n :: ns)  => symtab.get(n) map { decl => lookUpField(ns, decl.kind) }
      }
    }

    kind map { x: Type => x.widthExpr }
  }

  // Compute a new expression representing the width of this expression.
  // Return None if it cannot be determined.
  lazy val widthExpr: Option[Expr] = widthExpr(Map.empty[String, Decl])
  def widthExpr(symtab: scala.collection.Map[String, Decl]): Option[Expr] = {
    this match {
      case name: DottedName          => widthOfName(symtab, name)
      case ExprArrIndex(name, _)     => widthOfName(symtab, name)
      case _: ExprVecIndex           => ??? // TODO: handle IntVType properly
      case ReadCall(name)            => widthOfName(symtab, name)
      case Num(_, Some(width), _)    => Some(Expr(width))
      case Num(_, None, _)           => None
      case _: CallExpr               => Some(Expr(0))
      case Zxt(numbits, _)           => Some(numbits)
      case Sxt(numbits, _)           => Some(numbits)
      case _: DollarCall             => None
      case PipelineRead              => None
      case PipelineWrite             => None
      case _: WaitCall               => Some(Expr(0))
      case _: ValidCall              => Some(Expr(1))
      case _: WriteCall              => Some(Expr(0))
      case _: BinaryOp               => None
      case UnaryOp(_, lhs)           => lhs.widthExpr(symtab)
      case Bracket(content)          => content.widthExpr(symtab)
      case _: TernaryOp              => None
      case Slice(_, lidx, ":", ridx) => Some(lidx - ridx + 1) // TODO: assert lidx >= ridx
      case Slice(_, _, _, width)     => Some(width)
      case _: Literal                => None
      case ErrorExpr                 => None
      case BitRep(count, value) => value.widthExpr(symtab) match {
        case Some(w) => Some(count * w)
        case None    => Message.fatal("Cannot compute width of bit repetion")
      }
      case BitCat(terms) => {
        val exprs = for (term <- terms) yield {
          term.widthExpr(symtab) match {
            case Some(expr) => expr
            case None       => Message.fatal("Cannot compute width of bit concatenation operand")
          }
        }
        Some(exprs.reduce(_ + _))
      }
    }
  }

  // Compute a new expression representing the MSB of this expression.
  // Return None if it cannot be determined.
  def msbExpr: Option[Expr] = msbExpr(Map.empty[String, Decl])
  def msbExpr(symtab: scala.collection.Map[String, Decl]): Option[Expr] = {
    this match {
      case name: DottedName => widthOfName(symtab, name) map { width =>
        Slice(name, width - 1, "+:", Expr(1))
      }
      case vref @ ExprArrIndex(name, _) => widthOfName(symtab, name) map { width =>
        Slice(vref, width - 1, "+:", Expr(1))
      }
      case _: ExprVecIndex => ??? // TODO: handle IntVType properly
      case ReadCall(name) => widthOfName(symtab, name) map { width =>
        Slice(name, width - 1, "+:", Expr(1))
      }

      case Num(_, Some(width), value)    => Some(Num(false, Some(1), value >> (width - BigInt(1)).toInt))
      case Num(_, None, _)               => None
      case _: CallExpr                   => None
      case Zxt(_, expr)                  => None // TODO: if (numbits == expr.widthExpr.eval) expr.msbExpr else 0
      case Sxt(_, expr)                  => expr.msbExpr(symtab)
      case _: DollarCall                 => None
      case PipelineRead                  => None
      case PipelineWrite                 => None
      case _: WaitCall                   => None
      case _: ValidCall                  => None
      case _: WriteCall                  => None
      case _: BinaryOp                   => None
      case UnaryOp(_, lhs)               => None // TODO: could handle ~
      case Bracket(content)              => content.msbExpr(symtab)
      case _: TernaryOp                  => None
      case Slice(ref, lidx, ":", _)      => Some(Slice(ref, lidx, "+:", Expr(1)))
      case Slice(ref, base, "+:", width) => Some(Slice(ref, base + width - 1, "+:", Expr(1)))
      case Slice(ref, base, "-:", _)     => Some(Slice(ref, base, "+:", Expr(1)))
      case _: Slice                      => unreachable
      case BitRep(_, value)              => value.msbExpr(symtab)
      case BitCat(terms)                 => terms.head.msbExpr(symtab)
      case _: Literal                    => None
      case ErrorExpr                     => None
    }
  }

  def hasSideEffect: Boolean = this match {
    case _: CallExpr                => true
    case _: DollarCall              => true
    case _: ReadCall                => true
    case _: WriteCall               => true
    case PipelineRead               => true
    case PipelineWrite              => true
    case _: WaitCall                => true
    case ErrorExpr                  => true

    case _: DottedName              => false
    case _: Num                     => false
    case _: ValidCall               => false
    case _: Literal                 => false

    case ExprArrIndex(_, indices)   => indices exists { _.hasSideEffect }
    case ExprVecIndex(ref, indices) => ref.hasSideEffect || (indices exists { _.hasSideEffect })
    case Zxt(_, e)                  => e.hasSideEffect
    case Sxt(_, e)                  => e.hasSideEffect
    case BinaryOp(l, _, r)          => l.hasSideEffect || r.hasSideEffect
    case UnaryOp(_, e)              => e.hasSideEffect
    case Bracket(e)                 => e.hasSideEffect
    case TernaryOp(c, t, f)         => c.hasSideEffect || t.hasSideEffect || f.hasSideEffect
    case BitRep(_, e)               => e.hasSideEffect
    case BitCat(terms)              => terms exists { _.hasSideEffect }
    case Slice(_, l, _, r)          => l.hasSideEffect || r.hasSideEffect
  }

  def toVerilog: String = this match {
    case Num(false, None, value)        => s"'d${value}"
    case Num(true, None, value)         => value.toString
    case Num(false, Some(width), value) => if (width == 1) s"1'b${value}" else s"${width}'d${value}"
    case Num(true, Some(width), value)  => s"${width}'sd${value}"
    case _: CallExpr                    => ???
    case Zxt(numbits, expr)             => ???
    case Sxt(numbits, expr)             => ???
    case _: DollarCall                  => ???
    case _: ReadCall                    => ???
    case PipelineRead                   => ???
    case PipelineWrite                  => ???
    case _: WaitCall                    => ???
    case _: ValidCall                   => ???
    case _: WriteCall                   => ???
    case BinaryOp(lhs, op, rhs)         => s"(${lhs.toVerilog}) $op (${rhs.toVerilog})"
    case UnaryOp(_, lhs)                => ???
    case Bracket(content)               => s"(${content})"
    case TernaryOp(cond, lhs, rhs)      => ???
    case BitRep(count, value)           => ???
    case BitCat(terms)                  => ???
    case _: Slice                       => ???
    case DottedName(name :: Nil)        => name
    case x: DottedName                  => Message.ice(s"Cannot generate verilog for '$x'")
    case _: ExprArrIndex                => ???
    case _: ExprVecIndex                => ???
    case _: Literal                     => ???
    case ErrorExpr                      => "/*Error expression*/"
  }

  def kind(symtab: scala.collection.Map[String, Decl]): Option[Type] = this match {
    case _: Num          => ???
    case _: CallExpr     => ???
    case _: Zxt          => ???
    case _: Sxt          => ???
    case _: DollarCall   => ???
    case _: ReadCall     => ???
    case PipelineRead    => ???
    case PipelineWrite   => ???
    case _: WaitCall     => ???
    case _: ValidCall    => ???
    case _: WriteCall    => ???
    case _: BinaryOp     => ???
    case _: UnaryOp      => ???
    case _: Bracket      => ???
    case _: TernaryOp    => ???
    case _: BitRep       => ???
    case _: BitCat       => ???
    case _: Slice        => ???
    case _: DottedName   => ???
    case _: ExprArrIndex => ???
    case _: ExprVecIndex => ???
    case _: Literal      => ???
    case ErrorExpr       => ???
  }
}

trait ExprObjOps {
  def apply(n: Int) = Num(true, None, n)
  def apply(n: BigInt) = Num(true, None, n)
}
