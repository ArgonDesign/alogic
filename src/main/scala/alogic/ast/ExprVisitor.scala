////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Visitor to convert expression parse trees to expression ASTs
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import alogic.Antlr4Conversions._
import alogic.VScalarVisitor
import org.antlr.v4.runtime.ParserRuleContext
import alogic.antlr.VParser._
import alogic.Message

class ExprVisitor(symtab: Option[Symtab], typedefs: scala.collection.Map[String, Type])
    extends VScalarVisitor[Expr] { self =>

  // If applied to a commaexpr node, return a list of the constructed expressions
  def apply(ctx: CommaexprContext): List[Expr] = visit(ctx.expr)

  private[this] lazy val st = symtab match {
    case Some(x) => x
    case None    => unreachable
  }

  private[this] def const2Num(const: String) = Num(true, None, BigInt(const filter (_ != '_')))
  private[this] def tickn2Num(ctx: ParserRuleContext, tickn: String, width: Option[String]): Num = {
    assert(tickn(0) == '\'')
    val widthVal = width filter (_ != '_') map (BigInt(_))
    val signed = tickn(1) == 's'
    val baseChar = if (signed) tickn(2) else tickn(1)
    val base = baseChar match {
      case 'b' => 2
      case 'd' => 10
      case 'h' => 16
      case c   => Message.error(ctx, s"Unknown base '$c'"); 16
    }
    val rest = if (signed) tickn drop 3 else tickn drop 2
    val digits = rest filter (_ != '_')
    val value = BigInt(digits, base)
    // TODO: check value fits in width
    Num(signed, widthVal, value)
  }

  override def visitExprBracket(ctx: ExprBracketContext) = Bracket(visit(ctx.expr))
  override def visitExprUnary(ctx: ExprUnaryContext) = UnaryOp(ctx.unary_op.text, visit(ctx.expr))
  override def visitExprMulDiv(ctx: ExprMulDivContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprAddSub(ctx: ExprAddSubContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprShift(ctx: ExprShiftContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprCompare(ctx: ExprCompareContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprEqual(ctx: ExprEqualContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprBAnd(ctx: ExprBAndContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprBXor(ctx: ExprBXorContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprBOr(ctx: ExprBOrContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprAnd(ctx: ExprAndContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprOr(ctx: ExprOrContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprTernary(ctx: ExprTernaryContext) = TernaryOp(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2)))
  override def visitExprRep(ctx: ExprRepContext) = BitRep(visit(ctx.expr(0)), visit(ctx.expr(1)))
  override def visitExprCat(ctx: ExprCatContext) = BitCat(this(ctx.commaexpr))
  override def visitExprDollar(ctx: ExprDollarContext) = DollarCall(ctx.DOLLARID, this(ctx.commaexpr))
  override def visitExprTrue(ctx: ExprTrueContext) = Num(false, Some(1), 1)
  override def visitExprFalse(ctx: ExprFalseContext) = Num(false, Some(1), 0)
  override def visitExprTrickNum(ctx: ExprTrickNumContext) = tickn2Num(ctx, ctx.TICKNUM, None)
  override def visitExprConstTickNum(ctx: ExprConstTickNumContext) = tickn2Num(ctx, ctx.TICKNUM, Some(ctx.CONSTANT))
  override def visitExprConst(ctx: ExprConstContext) = const2Num(ctx.CONSTANT)
  override def visitExprLiteral(ctx: ExprLiteralContext) = Literal(ctx.LITERAL)

  override def visitExprId(ctx: ExprIdContext) = st(ctx, ctx.text) match {
    case Left(decl) => DottedName(decl.id :: Nil)
    case Right(id)  => DottedName(id :: Nil)
  }

  override def visitExprDot(ctx: ExprDotContext) = visit(ctx.ref) match {
    case DottedName(names) => DottedName(names ::: ctx.IDENTIFIER.text :: Nil)
    case _                 => Message.fatal(ctx, s"Cannot access member of '${ctx.ref.sourceText}'")
  }

  override def visitExprIndex(ctx: ExprIndexContext) = {
    def fail = Message.fatal(ctx, s"Cannot index expression '${ctx.ref.sourceText}'")
    val ref = visit(ctx.ref)
    val idx = visit(ctx.idx)
    // TODO: Could do some checks in here for valid indexing
    ref match {
      case x @ DottedName(name :: Nil) => st(ctx, name).left.get match {
        case (_: DeclArr | _: DeclVerilogArr) => ExprArrIndex(x, idx :: Nil)
        case _                                => ExprVecIndex(x, idx :: Nil)
      }
      case _: DottedName => ExprVecIndex(ref, idx :: Nil)
      case ExprArrIndex(DottedName(name :: Nil), indices) => st(ctx, name).left.get match {
        case DeclArr(_, _, dims) => {
          if (dims.size < indices.size) {
            ExprArrIndex(DottedName(name :: Nil), indices ::: idx :: Nil)
          } else {
            ExprVecIndex(ref, idx :: Nil)
          }
        }
        case _ => fail
      }
      //      case x: DottedName              => ArrayLookup(x, idx :: Nil)
      //      case ArrayLookup(name, indices) => ArrayLookup(name, indices ::: idx :: Nil)
      case _ => fail
    }
  }

  override def visitExprSlice(ctx: ExprSliceContext) = {
    visit(ctx.ref) match {
      case x @ (_: DottedName | _: ExprArrIndex) => Slice(x, visit(ctx.lidx), ctx.op, visit(ctx.ridx))
      case _                                     => Message.fatal(ctx, s"Cannot slice expression '${ctx.ref.sourceText}'")
    }
  }

  override def visitExprCall(ctx: ExprCallContext) = ctx.ref.text match {
    case "read"  => PipelineRead
    case "write" => PipelineWrite
    case _ => {
      val args = this(ctx.commaexpr)

      val names = visit(ctx.ref) match {
        case DottedName(names) => names
        case _                 => Message.fatal(ctx, s"Cannot call '${ctx.sourceText}'")
      }

      def checkargs(minArgs: Int, maxArgs: Int = 0, hint: String = "")(expr: => Expr) = {
        val minA = minArgs
        val maxA = minArgs max maxArgs

        lazy val nstr = names mkString "."

        if (!((minA to maxA) contains args.length)) {
          if (minA == maxA) {
            Message.error(ctx, s"'$nstr' takes exactly ${minA} arguments: '$nstr($hint)'")
          } else {
            Message.error(ctx, s"'$nstr' takes between ${minA} and ${maxA} arguments: '$nstr($hint)'")
          }
          ErrorExpr
        } else {
          expr
        }
      }

      names match {
        case name :: "read" :: Nil => checkargs(0) {
          ReadCall(DottedName(name :: Nil))
        }
        case name :: "write" :: Nil => checkargs(0, 1, "[value]") {
          WriteCall(DottedName(name :: Nil), args)
        }
        case name :: "wait" :: Nil => checkargs(0) {
          WaitCall(DottedName(name :: Nil))
        }
        case name :: ("valid" | "v") :: Nil => checkargs(0) {
          ValidCall(DottedName(name :: Nil))
        }
        case name :: Nil => checkargs(0, 0) {
          CallExpr(DottedName(name :: Nil), Nil)
        }
        case _ => {
          Message.error(ctx, s"Call of unknown function '${names mkString "."}(...)'"); ErrorExpr
        }
      }
    }
  }

  override def visitExprAt(ctx: ExprAtContext) = {
    val name = ctx.ATID.text
    val args = this(ctx.commaexpr)

    def checkargs(hint: String*)(expr: => Expr) = {
      val expected = hint.length
      if (args.length != expected) {
        Message.error(ctx, s"'$name' takes exactly ${expected} arguments: '$name(${hint mkString ", "})'"); ErrorExpr
      } else {
        expr
      }
    }

    name match {
      case "@zx" => checkargs("number of bits", "expression") {
        Zxt(args(0), args(1))
      }
      case "@sx" => checkargs("number of bits", "expression") {
        Sxt(args(0), args(1))
      }
      case _ => {
        Message.error(ctx, s"Unknown Alogic function '$name'"); ErrorExpr
      }
    }
  }

  override def visitExprAtBits(ctx: ExprAtBitsContext) = {
    val visitor = new KnownTypeVisitor(symtab, typedefs)
    visitor(ctx.known_type).widthExpr
  }
}
