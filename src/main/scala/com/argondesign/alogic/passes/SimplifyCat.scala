////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Unpack ExprCat instances:
//  - Rewrite StmtAssign(ExprCat, ExprCat) as multiple assignments if possible
//  - Rewrite Connect(ExprCat, ExprCat) as multiple connections if possible
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

final class SimplifyCat(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Return a list of pairwise equal-length sub-lists that can be assigned to each other
  private[this] def pairUp(
      loc: Loc,
      as: List[Expr],
      bs: List[Expr]
  ): List[(List[Expr], List[Expr])] = {
    def width(es: List[Expr]) = (es map { _.tpe.width }).sum

    assert(width(as) == width(bs))
    val pairs = ListBuffer[(List[Expr], List[Expr])]()

    @tailrec
    def loop(suba: List[Expr], subb: List[Expr], as: List[Expr], bs: List[Expr]): Unit = {
      val aw = width(suba)
      val bw = width(subb)
      if (aw == bw) {
        pairs.append((suba.reverse, subb.reverse))
        (as, bs) match {
          case (Nil, Nil)         => ()
          case (a :: at, b :: bt) => loop(List(a), List(b), at, bt)
          case _                  => unreachable
        }
      } else if (aw < bw) {
        loop(as.head :: suba, subb, as.tail, bs)
      } else {
        loop(suba, bs.head :: subb, as, bs.tail)
      }
    }

    loop(List(as.head), List(bs.head), as.tail, bs.tail)

    pairs.toList
  }

  override def transform(tree: Tree): Tree = tree match {
    case StmtAssign(ExprCat(parts), ExprInt(_, _, value)) =>
      val widths = parts map { _.tpe.width.toInt }
      val lsbs = widths.scanRight(BigInt(0))(_ + _).tail map { _.toInt }
      Thicket {
        List from {
          (parts lazyZip widths lazyZip lsbs) flatMap {
            case (expr, width, lsb) =>
              val signed = expr.tpe.isSigned
              val rhs = TypeAssigner {
                ExprInt(signed, width.toInt, value.extract(lsb, width, signed)) withLoc tree.loc
              }
              walk(TypeAssigner(StmtAssign(expr, rhs) withLoc tree.loc)) match {
                case Thicket(ts) => ts
                case t           => List(t)
              }
          }
        }
      }

    case StmtAssign(ExprCat(oLhss), ExprCat(oRhss)) =>
      // Do not simplify if a symbol appears on both sides (eg {a, b} = {b, a})
      // as in this case the rhs must be read atomically
      val lSymbols = (oLhss collect { case ExprSym(symbol) => symbol }).toSet
      val rSymbols = (oRhss collect { case ExprSym(symbol) => symbol }).toSet
      lazy val pairs = pairUp(tree.loc, oLhss, oRhss)
      if ((lSymbols intersect rSymbols).nonEmpty || pairs.lengthIs == 1) {
        tree
      } else {
        Thicket {
          pairs flatMap {
            case (lhss, rhss) =>
              val lhs = lhss match {
                case head :: Nil => head
                case _           => TypeAssigner(ExprCat(lhss) withLoc tree.loc)
              }
              val rhs = rhss match {
                case head :: Nil => head
                case _           => TypeAssigner(ExprCat(rhss) withLoc tree.loc)
              }
              walk(TypeAssigner(StmtAssign(lhs, rhs) withLoc tree.loc)) match {
                case Thicket(ts) => ts
                case t           => List(t)
              }
          }
        }
      }

    case EntConnect(ExprCat(oLhss), List(ExprCat(oRhss))) =>
      val pairs = pairUp(tree.loc, oLhss, oRhss)
      if (pairs.lengthIs == 1) {
        tree
      } else {
        Thicket {
          pairs flatMap {
            case (lhss, rhss) =>
              val lhs = lhss match {
                case head :: Nil => head
                case _           => TypeAssigner(ExprCat(lhss) withLoc tree.loc)
              }
              val rhs = rhss match {
                case head :: Nil => head
                case _           => TypeAssigner(ExprCat(rhss) withLoc tree.loc)
              }
              walk(TypeAssigner(EntConnect(lhs, List(rhs)) withLoc tree.loc)) match {
                case Thicket(ts) => ts
                case t           => List(t)
              }
          }
        }
      }

    case _ => tree
  }

}

object SimplifyCat extends EntityTransformerPass(declFirst = true) {
  val name = "simplify-cat"
  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = cc.simplifyCat
}
