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
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.transform.ReplaceTermRefs
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.PartialMatch._
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions
import scala.math.BigInt.int2bigInt

trait ExprOps { this: Expr =>

  final private def fix(tree: Tree)(implicit cc: CompilerContext): tree.type = {
    if (hasLoc) {
      tree withLoc loc
    }
    if (hasTpe) {
      TypeAssigner(tree)
    }
    tree
  }

  final private def mkBinary(op: String, rhs: Expr)(implicit cc: CompilerContext) = {
    fix(ExprBinary(this, op, rhs))
  }

  final private def mkSized(v: Int)(implicit cc: CompilerContext) = {
    if (tpe.underlying.isNum) {
      fix(ExprNum(tpe.isSigned, v))
    } else {
      fix(ExprInt(tpe.isSigned, tpe.width.toInt, v))
    }
  }

  final private def mkIndex(idx: Int)(implicit cc: CompilerContext) = {
    fix(ExprInt(false, clog2(tpe.shapeIter.next) max 1, idx))
  }

  final private def mkSliceLength(idx: Int)(implicit cc: CompilerContext) = {
    fix(ExprInt(false, clog2(tpe.shapeIter.next + 1), idx))
  }

  // Helpers to easily combine expression trees manually with other expressions
  // format: off
  final def *  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("*", rhs)
  final def /  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("/", rhs)
  final def %  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("%", rhs)
  final def +  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("+", rhs)
  final def -  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("-", rhs)
  final def << (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<<", rhs)
  final def >> (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">>", rhs)
  final def <<<(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<<<", rhs)
  final def >>>(rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">>>", rhs)
  final def &  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("&", rhs)
  final def ^  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("^", rhs)
  final def |  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("|", rhs)
  final def && (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("&&", rhs)
  final def || (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("||", rhs)
  final def <  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<", rhs)
  final def <= (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary("<=", rhs)
  final def >  (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">", rhs)
  final def >= (rhs: Expr)(implicit cc: CompilerContext): ExprBinary = mkBinary(">=", rhs)

  final def *  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this *   mkSized(rhs)
  final def /  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this /   mkSized(rhs)
  final def %  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this %   mkSized(rhs)
  final def +  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this +   mkSized(rhs)
  final def -  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this -   mkSized(rhs)
  final def << (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this <<  fix(Expr(rhs))
  final def >> (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this >>  fix(Expr(rhs))
  final def <<<(rhs: Int)(implicit cc: CompilerContext): ExprBinary = this <<< fix(Expr(rhs))
  final def >>>(rhs: Int)(implicit cc: CompilerContext): ExprBinary = this >>> fix(Expr(rhs))
  final def &  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this &   mkSized(rhs)
  final def ^  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this ^   mkSized(rhs)
  final def |  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this |   mkSized(rhs)
  final def && (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this &&  fix(Expr(rhs))
  final def || (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this ||  fix(Expr(rhs))
  final def <  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this <   mkSized(rhs)
  final def <= (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this <=  mkSized(rhs)
  final def >  (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this >   mkSized(rhs)
  final def >= (rhs: Int)(implicit cc: CompilerContext): ExprBinary = this >=  mkSized(rhs)

  final def index(idx: Expr)(implicit cc: CompilerContext): ExprIndex = fix(ExprIndex(this, idx))
  final def index(idx: Int)(implicit cc: CompilerContext): ExprIndex = this index mkIndex(idx)

  final def slice(lIdx: Expr, op: String, rIdx: Expr)(implicit cc: CompilerContext): ExprSlice = 
    fix(ExprSlice(this, lIdx, op, rIdx))
  final def slice(lIdx: Expr, op: String, rIdx: Int )(implicit cc: CompilerContext): ExprSlice = 
    this.slice(lIdx, op, if (op == ":") mkIndex(rIdx) else mkSliceLength(rIdx))
  final def slice(lIdx: Int,  op: String, rIdx: Expr)(implicit cc: CompilerContext): ExprSlice = 
    this.slice(mkIndex(lIdx), op, rIdx)
  final def slice(lIdx: Int,  op: String, rIdx: Int )(implicit cc: CompilerContext): ExprSlice = 
    this.slice(mkIndex(lIdx), op, if (op == ":") mkIndex(rIdx) else mkSliceLength(rIdx))

  final def sel(name: String)(implicit cc: CompilerContext): ExprSel = fix(ExprSel(this, name, Nil))
  final def call(args: List[Arg])(implicit cc: CompilerContext): ExprCall = fix(ExprCall(this, args))
  final def call(arg: Expr)(implicit cc: CompilerContext): ExprCall = this.call(fix(ArgP(arg)) :: Nil)

  final def cat(rhs: Expr)(implicit cc: CompilerContext): ExprCat = fix(ExprCat(List(this, rhs)))

  final def rep(cnt: Expr)(implicit cc: CompilerContext): ExprRep = fix(ExprRep(cnt, this))
  final def rep(cnt: Int)(implicit cc: CompilerContext): ExprRep = this rep fix(Expr(cnt))

  final def cast(kind: TypeFund)(implicit cc: CompilerContext): ExprCast = fix(ExprCast(kind, this))

  final def unary(op: String)(implicit cc: CompilerContext): ExprUnary = fix(ExprUnary(op, this))

  final def unary_+ : this.type = this
  final def unary_-(implicit cc: CompilerContext) : ExprUnary = unary("-")
  final def unary_~(implicit cc: CompilerContext) : ExprUnary = unary("~")
  final def unary_!(implicit cc: CompilerContext) : ExprUnary = unary("!")

  final def zx(width: Expr)(implicit cc: CompilerContext): ExprCall = cc.makeBuiltinCall("@zx", loc, List(width, this))
  final def zx(width: Int)(implicit cc: CompilerContext): ExprCall = this zx fix(Expr(width))

  final def sx(width: Expr)(implicit cc: CompilerContext): ExprCall = cc.makeBuiltinCall("@sx", loc, List(width, this))
  final def sx(width: Int)(implicit cc: CompilerContext): ExprCall = this sx fix(Expr(width))

  final def castUnsigned(implicit cc: CompilerContext): ExprCall = cc.makeBuiltinCall("$unsigned", loc, this :: Nil)
  final def castSigned(implicit cc: CompilerContext): ExprCall = cc.makeBuiltinCall("$signed", loc, this :: Nil)
  // format: on

  final def inc(implicit cc: CompilerContext): Expr = if (tpe.isPacked && tpe.width == 1) {
    ~this
  } else {
    this + 1
  }

  final def dec(implicit cc: CompilerContext): Expr = if (tpe.isPacked && tpe.width == 1) {
    ~this
  } else {
    this - 1
  }

  final def assign(expr: Expr)(implicit cc: CompilerContext): StmtAssign =
    fix(StmtAssign(this, expr))
  final def assign(value: Int)(implicit cc: CompilerContext): StmtAssign =
    this assign fix(ExprInt(tpe.isSigned, tpe.width.toInt, value))

  // Is this expression shaped as a valid lvalue expression
  lazy val isLValueExpr: Boolean = this forall {
    case _: ExprRef               => true
    case _: ExprSym               => true
    case ExprIndex(expr, _)       => expr.isLValueExpr
    case ExprSlice(expr, _, _, _) => expr.isLValueExpr
    case ExprSel(expr, _, _)      => expr.isLValueExpr
    case ExprCat(parts)           => parts forall { _.isLValueExpr }
    case _                        => false
  }

  // Is this expression a known constant
  def isKnownConst(implicit cc: CompilerContext): Boolean = this match {
    case _: ExprNum  => true
    case _: ExprInt  => true
    case _: ExprStr  => true
    case _: ExprType => true
    case ExprSym(symbol) =>
      symbol.kind match {
        case _: TypeConst => true
        case _: TypeParam => unreachable
        case _: TypeGen   => true
        case _ =>
          symbol.decl match {
            case _: DeclVal => symbol.init.get.isKnownConst
            case _          => false
          }
      }
    case ExprUnary(_, expr)      => expr.isKnownConst
    case ExprBinary(lhs, _, rhs) => lhs.isKnownConst && rhs.isKnownConst
    case ExprCond(cond, thenExpr, elseExpr) =>
      thenExpr.isKnownConst && elseExpr.isKnownConst && (cond.isKnownConst || thenExpr.simplify == elseExpr.simplify)
    case ExprRep(count, expr)   => count.isKnownConst && expr.isKnownConst
    case ExprCat(parts)         => parts forall { _.isKnownConst }
    case ExprIndex(expr, index) => expr.isKnownConst && index.isKnownConst
    case ExprSlice(expr, lIdx, _, rIdx) =>
      expr.isKnownConst && lIdx.isKnownConst && rIdx.isKnownConst
    case ExprSel(expr, _, idxs) => expr.isKnownConst && (idxs forall { _.isKnownConst })
    case call @ ExprCall(ExprSym(symbol), _) if symbol.isBuiltin =>
      cc.isKnownConstBuiltinCall(call)
    case ExprCast(_, expr) => expr.isKnownConst
    case ExprOld(expr)     => expr.isKnownConst
    case _                 => false
  }

  final def isPure(implicit cc: CompilerContext): Boolean = {
    def p(expr: Expr): Boolean = expr match {
      case call @ ExprCall(tgt, _) =>
        tgt.tpe match {
          case TypePolyFunc(symbol, _) if symbol.isBuiltin    => cc.isPureBuiltinCall(call)
          case TypeCombFunc(symbol, _, _) if symbol.isBuiltin => cc.isPureBuiltinCall(call)
          case _                                              => false
        }
      case ExprUnary(_, e)         => p(e)
      case ExprBinary(l, _, r)     => p(l) && p(r)
      case ExprCond(c, t, e)       => p(c) && p(t) && p(e)
      case ExprRep(_, e)           => p(e)
      case ExprCat(ps)             => ps forall p
      case ExprIndex(e, i)         => p(e) && p(i)
      case ExprSlice(e, _, ":", _) => p(e)
      case ExprSlice(e, l, _, _)   => p(e) && p(l)
      case ExprSel(e, _, _)        => p(e)
      case _: ExprRef              => true
      case _: ExprSym              => true
      case _: ExprOld              => true
      case _: ExprThis             => true
      case _: ExprType             => true
      case ExprCast(_, e)          => p(e)
      case _: ExprInt              => true
      case _: ExprNum              => true
      case _: ExprStr              => true
      case _: ExprError            => true
    }
    p(this)
  }

  final protected var _simplified: Expr = null

  // Simplify this expression
  final def simplify(implicit cc: CompilerContext): Expr = {
    if (_simplified == null) {
      // Compute the simplified expression
      _simplified = (new TreeExt(this)).normalize rewrite cc.simpifyExpr
      // The simplified expression cannot be simplified further
      _simplified._simplified = _simplified
    }
    _simplified
  }

  final protected var _simplifiedLValue: Expr = null

  final def simplifyLValue(implicit cc: CompilerContext): Expr = {
    if (_simplifiedLValue == null) {
      // Compute the simplified expression
      _simplifiedLValue = this match {
        case _: ExprSym        => this
        case ExprCat(p :: Nil) => p
        case ExprCat(ps) =>
          TypeAssigner {
            ExprCat {
              ps flatMap {
                _.simplifyLValue match {
                  case ExprCat(es) => es
                  case e           => List(e)
                }
              }
            } withLoc loc
          }
        case _: ExprIndex => simplify
        case _: ExprSlice => simplify
        case _: ExprSel   => this
        case _            => unreachable
      }
      // The simplified expression cannot be simplified further
      _simplifiedLValue._simplifiedLValue = _simplifiedLValue
    }
    _simplifiedLValue
  }

  // Rewrite expression using bindings provided
  def given(bindings: Symbol => Option[Expr])(implicit cc: CompilerContext): Expr =
    this rewrite new ReplaceTermRefs(bindings)

  def given(bindings: Bindings)(implicit cc: CompilerContext): Expr = this.given(bindings.get _)

  // Value of this expression if it can be determined right now, otherwise None
  def value(implicit cc: CompilerContext): Option[BigInt] = simplify match {
    case ExprNum(_, value)    => Some(value)
    case ExprInt(_, _, value) => Some(value)
    case _                    => None
  }

  // True if this we know the value of this expression (same as value.isDefined)
  def isKnown(implicit cc: CompilerContext): Boolean = value.isDefined
}

trait ExprObjOps { self: Expr.type =>
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

  final object <<< {
    def unapply(expr: ExprBinary) = if (expr.op == "<<<") Some((expr.lhs, expr.rhs)) else None
  }

  final object >>> {
    def unapply(expr: ExprBinary) = if (expr.op == ">>>") Some((expr.lhs, expr.rhs)) else None
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

  final object < {
    def unapply(expr: ExprBinary) = if (expr.op == "<") Some((expr.lhs, expr.rhs)) else None
  }

  final object <= {
    def unapply(expr: ExprBinary) = if (expr.op == "<=") Some((expr.lhs, expr.rhs)) else None
  }

  final object > {
    def unapply(expr: ExprBinary) = if (expr.op == ">") Some((expr.lhs, expr.rhs)) else None
  }

  final object >= {
    def unapply(expr: ExprBinary) = if (expr.op == ">=") Some((expr.lhs, expr.rhs)) else None
  }

  // Extractor for instance port selects
  final object InstancePortSel {

    def unapply(expr: ExprSel)(implicit cc: CompilerContext): Option[(Symbol, Symbol)] =
      expr match {
        case ExprSel(ExprSym(symbol), sel, idxs) =>
          assert(idxs.isEmpty, "InstancePortSel cannot be used before elaboration")
          symbol.kind match {
            case kind: TypeEntity => kind(sel) map { (symbol, _) }
            case _                => None
          }
        case _ => None
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
