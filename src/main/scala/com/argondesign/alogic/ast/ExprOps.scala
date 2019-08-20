////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Common members of ast.Trees.Expr
// These are factored out into a separate file to keep ast.Trees readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.passes.FoldExpr
import com.argondesign.alogic.transform.ReplaceTermRefs
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.PartialMatch._

import scala.language.implicitConversions
import scala.math.BigInt.int2bigInt

trait ExprOps { this: Expr =>

  private final def fix(expr: Expr)(implicit cc: CompilerContext): expr.type = {
    if (hasLoc) {
      expr withLoc loc
    }
    if (hasTpe) {
      TypeAssigner(expr)
    }
    expr
  }

  private final def mkBinary(op: String, rhs: Expr)(implicit cc: CompilerContext) = {
    fix(ExprBinary(this, op, rhs))
  }

  private final def mkSized(v: Int)(implicit cc: CompilerContext) = {
    if (tpe.underlying.isNum) fix(Expr(v)) else fix(ExprInt(tpe.isSigned, tpe.width, v))
  }

  private final def mkIndex(idx: Int)(implicit cc: CompilerContext) = {
    fix(ExprInt(false, clog2(tpe.shapeIter.next) max 1, idx))
  }

  // Helpers to easily combine expression trees manually with other expressions
  // format: off
  final def *(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("*", rhs)
  final def /(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("/", rhs)
  final def %(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("%", rhs)
  final def +(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("+", rhs)
  final def -(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("-", rhs)
  final def <<(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<<", rhs)
  final def >>(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">>", rhs)
  final def <<<(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<<<", rhs)
  final def >>>(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">>>", rhs)
  final def &(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("&", rhs)
  final def ^(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("^", rhs)
  final def |(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("|", rhs)
  final def &&(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("&&", rhs)
  final def ||(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("||", rhs)
  final def <(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<", rhs)
  final def <=(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<=", rhs)
  final def >(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">", rhs)
  final def >=(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">=", rhs)

  final def *(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("*", mkSized(rhs))
  final def /(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("/", mkSized(rhs))
  final def %(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("%", mkSized(rhs))
  final def +(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("+", mkSized(rhs))
  final def -(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("-", mkSized(rhs))
  final def <<(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("<<", fix(Expr(rhs)))
  final def >>(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary(">>", fix(Expr(rhs)))
  final def <<<(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("<<<", fix(Expr(rhs)))
  final def >>>(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary(">>>", fix(Expr(rhs)))
  final def &(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("&", mkSized(rhs))
  final def ^(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("^", mkSized(rhs))
  final def |(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("|", mkSized(rhs))
  final def &&(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("&&", fix(Expr(rhs)))
  final def ||(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("||", fix(Expr(rhs)))
  final def <(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("<", fix(Expr(rhs)))
  final def <=(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary("<=", fix(Expr(rhs)))
  final def >(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary(">", fix(Expr(rhs)))
  final def >=(rhs: Int)(implicit cc: CompilerContext): ExprBinary = mkBinary(">=", fix(Expr(rhs)))

  final def index(idx: Expr)(implicit cc: CompilerContext): ExprIndex = fix(ExprIndex(this, idx))
  final def index(idx: Int)(implicit cc: CompilerContext): ExprIndex = fix(ExprIndex(this, mkIndex(idx)))

  final def slice(lidx: Expr, op: String, ridx: Expr)(implicit cc: CompilerContext): ExprSlice = fix(ExprSlice(this, lidx, op, ridx))
  final def slice(lidx: Expr, op: String, ridx: Int)(implicit cc: CompilerContext): ExprSlice = fix(ExprSlice(this, lidx, op, mkIndex(ridx)))
  final def slice(lidx: Int, op: String, ridx: Expr)(implicit cc: CompilerContext): ExprSlice = fix(ExprSlice(this, mkIndex(lidx), op, ridx))
  final def slice(lidx: Int, op: String, ridx: Int)(implicit cc: CompilerContext): ExprSlice = fix(ExprSlice(this, mkIndex(lidx), op, mkIndex(ridx)))

  final def select(name: String)(implicit cc: CompilerContext): ExprSelect = fix(ExprSelect(this, name))
  final def call(args: List[Expr])(implicit cc: CompilerContext): ExprCall = fix(ExprCall(this, args))

  final def cat(rhs: Expr)(implicit cc: CompilerContext): ExprCat = fix(ExprCat(List(this, rhs)))

  final def rep(cnt: Expr)(implicit cc: CompilerContext): ExprRep = fix(ExprRep(cnt, this))
  final def rep(cnt: Int)(implicit cc: CompilerContext): ExprRep = fix(ExprRep(fix(Expr(cnt)), this))

  final def cast(kind: Type)(implicit cc: CompilerContext): ExprCast = fix(ExprCast(kind, this))

  final def zx(width: Expr)(implicit cc: CompilerContext): ExprCall = cc.makeBuiltinCall("@zx", loc, List(width, this))
  final def zx(width: Int)(implicit cc: CompilerContext): ExprCall = this zx fix(Expr(width))

  final def sx(width: Expr)(implicit cc: CompilerContext): ExprCall = cc.makeBuiltinCall("@sx", loc, List(width, this))
  final def sx(width: Int)(implicit cc: CompilerContext): ExprCall = this sx fix(Expr(width))

  final def unary(op: String)(implicit cc: CompilerContext): ExprUnary = fix(ExprUnary(op, this))

  final def unary_+ : this.type = this
  final def unary_-(implicit cc: CompilerContext) : ExprUnary = fix(ExprUnary("-", this))
  final def unary_~(implicit cc: CompilerContext) : ExprUnary = fix(ExprUnary("~", this))
  final def unary_!(implicit cc: CompilerContext) : ExprUnary = fix(ExprUnary("!", this))
  // format: on

  // Is this expression shaped as a valid type expression
  lazy val isTypeExpr: Boolean = this forall {
    case _: ExprIdent        => true
    case _: ExprRef          => true
    case _: ExprType         => true
    case ExprSelect(expr, _) => expr.isTypeExpr
    case _                   => false
  }

  // Is this expression shaped as a valid lvalue expression
  lazy val isLValueExpr: Boolean = this forall {
    case _: ExprIdent             => true
    case _: ExprRef               => true
    case ExprIndex(expr, _)       => expr.isLValueExpr
    case ExprSlice(expr, _, _, _) => expr.isLValueExpr
    case ExprSelect(expr, _)      => expr.isLValueExpr
    case ExprCat(parts)           => parts forall { _.isLValueExpr }
    case _                        => false
  }

  // Is this expression shaped as a valid port reference expression
  lazy val isPortRefExpr: Boolean = this match {
    case _: ExprIdent                => true
    case _: ExprRef                  => true
    case ExprSelect(_: ExprIdent, _) => true
    case ExprSelect(_: ExprRef, _)   => true
    case _                           => false
  }

  // Is this expression a known constant
  def isKnownConst(implicit cc: CompilerContext): Boolean = this match {
    case _: ExprNum  => true
    case _: ExprInt  => true
    case _: ExprStr  => true
    case _: ExprType => true
    case ExprRef(symbol) =>
      symbol.kind match {
        case _: TypeConst => true
        case _: TypeGen   => true
        case _            => false
      }
    case ExprUnary(_, expr)      => expr.isKnownConst
    case ExprBinary(lhs, _, rhs) => lhs.isKnownConst && rhs.isKnownConst
    case ExprTernary(cond, thenExpr, elseExpr) =>
      thenExpr.isKnownConst && elseExpr.isKnownConst && (cond.isKnownConst || thenExpr.simplify == elseExpr.simplify)
    case ExprRep(count, expr)   => count.isKnownConst && expr.isKnownConst
    case ExprCat(parts)         => parts forall { _.isKnownConst }
    case ExprIndex(expr, index) => expr.isKnownConst && index.isKnownConst
    case ExprSlice(expr, lidx, op, ridx) =>
      expr.isKnownConst && lidx.isKnownConst && ridx.isKnownConst
    case ExprSelect(expr, _) => expr.isKnownConst
    case call @ ExprCall(ExprRef(symbol), _) if symbol.isBuiltin =>
      cc.isKnownConstBuiltinCall(call)
    case ExprCast(_, expr) => expr.isKnownConst
    case _                 => false
  }

  // Simplify this expression
  def simplify(implicit cc: CompilerContext): Expr = {
    val simple = this.normalize rewrite {
      new FoldExpr(foldRefs = true)
    }
    simple.asInstanceOf[Expr]
  }

  // Rewrite expression using bindings provided
  def given(bindings: Bindings)(implicit cc: CompilerContext): Expr = {
    (this rewrite new ReplaceTermRefs(bindings)).asInstanceOf[Expr]
  }

  // Value of this expression if it can be determined right now, otherwise None
  def value(implicit cc: CompilerContext): Option[BigInt] = simplify match {
    case ExprNum(_, value)    => Some(value)
    case ExprInt(_, _, value) => Some(value)
    case _                    => None
  }
}

trait ObjectExprOps {
  // Helpers to easily create ExprNum from integers
  final def apply(n: Int): ExprNum = ExprNum(false, n)
  final def apply(n: BigInt): ExprNum = ExprNum(false, n)

  object ImplicitConversions {
    // Implicit conversion for Int to ExprNum
    implicit final def int2ExprNum(n: Int): ExprNum = apply(n)
  }

  // And extractor so we can match against the the same as above
  final def unapply(num: ExprNum): Option[Int] = if (!num.signed) Some(num.value.toInt) else None

  // Extractors to match operators naturally
  final object * {
    def unapply(expr: ExprBinary) = if (expr.op == "*") Some((expr.lhs, expr.rhs)) else None
  }
  final object / {
    def unapply(expr: ExprBinary) = if (expr.op == "/") Some((expr.lhs, expr.rhs)) else None
  }
  final object % {
    def unapply(expr: ExprBinary) = if (expr.op == "%") Some((expr.lhs, expr.rhs)) else None
  }
  final object + {
    def unapply(expr: ExprBinary) = if (expr.op == "+") Some((expr.lhs, expr.rhs)) else None
  }
  final object - {
    def unapply(expr: ExprBinary) = if (expr.op == "-") Some((expr.lhs, expr.rhs)) else None
  }
  final object << {
    def unapply(expr: ExprBinary) = if (expr.op == "<<") Some((expr.lhs, expr.rhs)) else None
  }
  final object >> {
    def unapply(expr: ExprBinary) = if (expr.op == ">>") Some((expr.lhs, expr.rhs)) else None
  }
  final object >>> {
    def unapply(expr: ExprBinary) = if (expr.op == ">>>") Some((expr.lhs, expr.rhs)) else None
  }
  final object <<< {
    def unapply(expr: ExprBinary) = if (expr.op == "<<<") Some((expr.lhs, expr.rhs)) else None
  }
  final object & {
    def unapply(expr: ExprBinary) = if (expr.op == "&") Some((expr.lhs, expr.rhs)) else None
  }
  final object ^ {
    def unapply(expr: ExprBinary) = if (expr.op == "^") Some((expr.lhs, expr.rhs)) else None
  }
  final object | {
    def unapply(expr: ExprBinary) = if (expr.op == "|") Some((expr.lhs, expr.rhs)) else None
  }
  final object && {
    def unapply(expr: ExprBinary) = if (expr.op == "&&") Some((expr.lhs, expr.rhs)) else None
  }
  final object || {
    def unapply(expr: ExprBinary) = if (expr.op == "||") Some((expr.lhs, expr.rhs)) else None
  }

  // Extractor for instance port references
  final object InstancePortRef {
    def unapply(expr: ExprSelect): Option[(TermSymbol, TermSymbol)] = expr partialMatch {
      case ExprSelect(ExprRef(iSymbol: TermSymbol), sel) if iSymbol.kind.isInstance =>
        (iSymbol, iSymbol.kind.asInstanceOf[TypeInstance].portSymbol(sel).get)
    }
  }

  // Extractor for integral values (ExprInt or ExprNum)
  final object Integral {
    def unapply(expr: Expr): Option[(Boolean, Option[Int], BigInt)] = expr partialMatch {
      case ExprNum(signed, value)        => (signed, None, value)
      case ExprInt(signed, width, value) => (signed, Some(width), value)
    }
  }
}
