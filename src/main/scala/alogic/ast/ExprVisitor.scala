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

  private[this] implicit val isymtab = symtab

  // If applied to a commaexpr node, return a list of the constructed expressions
  def apply(ctx: CommaexprContext): List[Expr] = visit(ctx.expr)

  private[this] lazy val st = symtab match {
    case Some(x) => x
    case None    => unreachable
  }

  private[this] def const2Num(ctx: ParserRuleContext, const: String) = Num(Attr(ctx), true, None, BigInt(const filter (_ != '_')))
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
    Num(Attr(ctx), signed, widthVal, value)
  }

  override def visitExprBracket(ctx: ExprBracketContext) = Bracket(Attr(ctx), visit(ctx.expr))
  override def visitExprUnary(ctx: ExprUnaryContext) = UnaryOp(Attr(ctx), ctx.unary_op.text, visit(ctx.expr))
  override def visitExprMulDiv(ctx: ExprMulDivContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprAddSub(ctx: ExprAddSubContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprShift(ctx: ExprShiftContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprCompare(ctx: ExprCompareContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprEqual(ctx: ExprEqualContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprBAnd(ctx: ExprBAndContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprBXor(ctx: ExprBXorContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprBOr(ctx: ExprBOrContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprAnd(ctx: ExprAndContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprOr(ctx: ExprOrContext) = BinaryOp(Attr(ctx), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
  override def visitExprTernary(ctx: ExprTernaryContext) = TernaryOp(Attr(ctx), visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2)))
  override def visitExprRep(ctx: ExprRepContext) = BitRep(Attr(ctx), visit(ctx.expr(0)), visit(ctx.expr(1)))
  override def visitExprCat(ctx: ExprCatContext) = BitCat(Attr(ctx), this(ctx.commaexpr))
  override def visitExprDollar(ctx: ExprDollarContext) = DollarCall(Attr(ctx), ctx.DOLLARID, this(ctx.commaexpr))
  override def visitExprTrue(ctx: ExprTrueContext) = Num(Attr(ctx), false, Some(1), 1)
  override def visitExprFalse(ctx: ExprFalseContext) = Num(Attr(ctx), false, Some(1), 0)
  override def visitExprTrickNum(ctx: ExprTrickNumContext) = tickn2Num(ctx, ctx.TICKNUM, None)
  override def visitExprConstTickNum(ctx: ExprConstTickNumContext) = tickn2Num(ctx, ctx.TICKNUM, Some(ctx.CONSTANT))
  override def visitExprConst(ctx: ExprConstContext) = const2Num(ctx, ctx.CONSTANT)
  override def visitExprLiteral(ctx: ExprLiteralContext) = Literal(Attr(ctx), ctx.LITERAL)

  override def visitExprId(ctx: ExprIdContext) = st(ctx, ctx.text) match {
    case Left(decl) => DottedName(Attr(ctx), decl.id :: Nil)
    case Right(id)  => DottedName(Attr(ctx), id :: Nil)
  }

  override def visitExprDot(ctx: ExprDotContext) = visit(ctx.ref) match {
    case DottedName(_, names) => DottedName(Attr(ctx), names ::: ctx.IDENTIFIER.text :: Nil)
    case _                    => Message.fatal(ctx, s"Cannot access member of '${ctx.ref.sourceText}'")
  }

  override def visitExprIndex(ctx: ExprIndexContext) = {
    def fail = Message.fatal(ctx, s"Cannot index expression '${ctx.ref.sourceText}'")
    val ref = visit(ctx.ref)
    val idx = visit(ctx.idx)
    val attr = Attr(ctx)
    // TODO: Could do some checks in here for valid indexing
    ref match {
      case x @ DottedName(_, name :: Nil) => st(ctx, name).left.get match {
        case (_: DeclArr | _: DeclVerilogArr) => ExprArrIndex(attr, x, idx :: Nil)
        case _                                => ExprVecIndex(attr, x, idx :: Nil)
      }
      case _: DottedName => ExprVecIndex(attr, ref, idx :: Nil)
      case ExprArrIndex(_, dn @ DottedName(_, name :: Nil), indices) => st(ctx, name).left.get match {
        case DeclArr(_, _, dims) => {
          if (dims.size < indices.size) {
            ExprArrIndex(attr, dn, indices ::: idx :: Nil)
          } else {
            ExprVecIndex(attr, ref, idx :: Nil)
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
      case x @ (_: DottedName | _: ExprArrIndex) => Slice(Attr(ctx), x, visit(ctx.lidx), ctx.op, visit(ctx.ridx))
      case _                                     => Message.fatal(ctx, s"Cannot slice expression '${ctx.ref.sourceText}'")
    }
  }

  override def visitExprCall(ctx: ExprCallContext) = {
    val attr = Attr(ctx)
    ctx.ref.text match {
      case "read"  => PipelineRead(attr)
      case "write" => PipelineWrite(attr)
      case _ => {
        val args = this(ctx.commaexpr)

        val names = visit(ctx.ref) match {
          case DottedName(_, names) => names
          case _                    => Message.fatal(ctx, s"Cannot call '${ctx.sourceText}'")
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
            ErrorExpr(attr)
          } else {
            expr
          }
        }

        names match {
          case name :: "read" :: Nil => checkargs(0) {
            ReadCall(attr, DottedName(attr, name :: Nil))
          }
          case name :: "write" :: Nil => checkargs(0, 1, "[value]") {
            WriteCall(attr, DottedName(attr, name :: Nil), args)
          }
          case name :: "wait" :: Nil => checkargs(0) {
            WaitCall(attr, DottedName(attr, name :: Nil))
          }
          case name :: ("valid" | "v") :: Nil => checkargs(0) {
            ValidCall(attr, DottedName(attr, name :: Nil))
          }
          case name :: Nil => checkargs(0, 0) {
            CallExpr(attr, DottedName(attr, name :: Nil), Nil)
          }
          case _ => {
            Message.error(ctx, s"Call of unknown function '${names mkString "."}(...)'"); ErrorExpr(attr)
          }
        }
      }
    }
  }

  override def visitExprAt(ctx: ExprAtContext) = {
    val name = ctx.ATID.text
    val args = this(ctx.commaexpr)
    val attr = Attr(ctx)

    def checkargs(hint: String*)(expr: => Expr) = {
      val expected = hint.length
      if (args.length != expected) {
        Message.error(ctx, s"'$name' takes exactly ${expected} arguments: '$name(${hint mkString ", "})'")
        ErrorExpr(attr)
      } else {
        expr
      }
    }

    name match {
      case "@zx" => checkargs("number of bits", "expression") {
        Zxt(attr, args(0), args(1))
      }
      case "@sx" => checkargs("number of bits", "expression") {
        Sxt(attr, args(0), args(1))
      }
      case _ => {
        Message.error(ctx, s"Unknown Alogic function '$name'")
        ErrorExpr(attr)
      }
    }
  }

  override def visitExprAtBits(ctx: ExprAtBitsContext) = {
    val visitor = new KnownTypeVisitor(symtab, typedefs)
    visitor(ctx.known_type).widthExpr
  }
}
