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
// Further port usage checks
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class PortCheckB(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  private[this] def multipleDriverError(rhs: Expr, loc: Loc, sliceOrIndexStr: String): Unit =
    cc.error(rhs, s"Port has multiple drivers. Other '->' is at:", loc.prefix)

  // TODO: Add back correct skip condition
//  override def skip(tree: Tree): Boolean = tree match {
//    case _: Root       => false
//    case _: RizDecl    => false
//    case _: Decl       => false
//    case _: DescEntity => false
//    case _: EntDecl    => false
//    case _             => true
//  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {

      case entity: DefnEntity => {
        //////////////////////////////////////////////////////////////////////////
        // Check multiple drivers
        //////////////////////////////////////////////////////////////////////////

        //
        // At this stage we have already done a FoldExpr pass, so indexes/slices
        // of slices have been eradicated. It's still possible to have indexes/slices
        // of indexes due to the existence of vector types.
        //

        // IndexRange is a help class, underlying is either a range Some(upper, lower), or
        // None, indicating that all bits of an expression were seen
        class IndexRange(val underlying: Option[(BigInt, BigInt)]) {
          def overlap(other: IndexRange): Option[IndexRange] = (this, other) match {
            case (IndexRangeSlice(u1, l1), IndexRangeSlice(u2, l2)) => {
              val highestLower = l1 max l2
              val lowestUpper = u1 min u2
              if (highestLower <= lowestUpper) Some(IndexRangeSlice(lowestUpper, highestLower))
              else None
            }
            case (IndexRangeSlice(_, _), IndexRangeAll()) => Some(this)
            case (IndexRangeAll(), IndexRangeSlice(_, _)) => Some(other)
            case (IndexRangeAll(), IndexRangeAll())       => Some(this)
          }

          override def toString: String = this match {
            case IndexRangeAll()               => ""
            case IndexRangeIndex(index)        => s"[${index}]"
            case IndexRangeSlice(upper, lower) => s"[${upper}:${lower}]"
          }
        }

        object IndexRangeAll {
          def apply(): IndexRange = new IndexRange(None)
          def unapply(arg: IndexRange): Boolean = arg.underlying.isEmpty
        }
        object IndexRangeIndex {
          def apply(index: BigInt): IndexRange = new IndexRange(Some((index, index)))
          def unapply(arg: IndexRange): Option[BigInt] = arg.underlying match {
            case Some((upper, lower)) if upper == lower => Some(upper)
            case _                                      => None
          }
        }
        object IndexRangeSlice {
          def apply(upper: BigInt, lower: BigInt): IndexRange = new IndexRange(Some((upper, lower)))
          def unapply(arg: IndexRange): Option[(BigInt, BigInt)] = arg.underlying match {
            case Some((upper, lower)) => Some((upper, lower))
            case _                    => None
          }
        }
        // Map of expressions appearing on the right of a connect that is declared by
        // us to a list of (Expr, Option[IndexRange]) pairs, where the Expr part of the
        // tuple is used only for error reporting, since it keeps track of the locations.
        // Since there are no indexes/slices of slices, none of the expressions in this
        // map will contain ExprSlice. ExprIndex are possible though and are dealt with
        // accordingly in checkWiderSinks.
        val sinks = mutable.Map[Expr, mutable.ListBuffer[(Expr, IndexRange)]]()

        // First populate sinks from all connects in this entity
        for {
          EntConnect(_, rhss) <- entity.connects
          rhs <- rhss
          if !rhs.tpe.isEntity
        } {

          def collectSinks(expr: Expr): Unit = expr match {
            case ExprCat(parts) => parts foreach collectSinks
            case ExprIndex(e, idxe) => {
              val idx = idxe.value.get
              val newSeenIndex = (e, IndexRangeIndex(idx))
              sinks.getOrElseUpdate(e, mutable.ListBuffer.empty).append(newSeenIndex)
            }
            case ExprSlice(e, lidxe, op, ridxe) => {
              val (lidx, ridx) = (lidxe.value.get, ridxe.value.get)
              val (upper, lower) = op match {
                case ":"  => (lidx, ridx)
                case "+:" => (lidx + ridx - 1, lidx)
                case "-:" => (lidx, lidx - ridx + 1)
              }
              val newSeenIndexRange = (e, IndexRangeSlice(upper, lower))
              sinks.getOrElseUpdate(e, mutable.ListBuffer.empty).append(newSeenIndexRange)
            }
            case e => {
              val newSeenWholeExpr = (e, IndexRangeAll())
              sinks.getOrElseUpdate(e, mutable.ListBuffer.empty).append(newSeenWholeExpr)
            }
          }

          collectSinks(rhs)
        }

        // Now iterate through sinks and check overlaps
        for {
          (expr, listOfAppearances) <- sinks
        } {

          // First check listOfAppearances doesn't contain any clashes
          @tailrec
          def checkListOverlaps(loa: List[(Expr, IndexRange)]): Unit = loa match {
            case List((_, IndexRangeAll())) => () // Single appearance of whole Expr is fine
            case (e1, ir1) :: tail =>
              for { (e2, ir2) <- tail } {
                (ir1 overlap ir2) foreach { overlap =>
                  multipleDriverError(e2, e1.loc, overlap.toString)
                }
              }
              checkListOverlaps(tail)
            case List() => () // Recursion base case
          }

          checkListOverlaps(listOfAppearances.toList)

          // Then check no expressions for which expr is a subset appear elsewhere in sinks.
          // Examples:
          // | expr      | Possible other entries in sinks to check
          // |-----------|--------------------------------------------------------------------------
          // | x.y       | x
          // | x.y.z     | x.y and x
          // | x[2]      | x (and check that corresponding listOfAppearances doesn't overlap with 0)
          // | x[1][2]   | x[1] (and check that x[1] listOfAppearances doesn't overlap with 2)
          // |           |     and x (and check that x listOfAppearances doesn't overlap with 1)
          //
          def overlapErrorAtLoc(otherLoc: Loc): Unit = listOfAppearances foreach {
            case (e, indexRange) => multipleDriverError(e, otherLoc, indexRange.toString)
          }

          @tailrec
          def checkWiderSinks(e_narrow: Expr): Unit = e_narrow match {
            case ExprSelect(e_wider, _, _) => {
              // E.g. e_narrow == x.y.z so check that e_wider (= x.y) doesn't have an entry in sinks
              for {
                loa <- sinks.get(e_wider)
                (otherExpr, _) <- loa
              } {
                overlapErrorAtLoc(otherExpr.loc)
              }
              // Now checkWiderSinks on e_wider = x.y
              checkWiderSinks(e_wider)
            }
            case ExprIndex(e_wider, idxe) => {
              val idx = idxe.value.get
              // E.g. e_narrow == x.y[idx] so check that we haven't seen e_wider (= x.y) or
              // a slice of e_wider overlapping with idx.
              val indexRangeNarrow = IndexRangeIndex(idx)
              for {
                loa <- sinks.get(e_wider)
                (otherExpr, otherIndexRange) <- loa
              } {
                (indexRangeNarrow overlap otherIndexRange) foreach { _ =>
                  overlapErrorAtLoc(otherExpr.loc)
                }
              }
              // Now checkWiderSinks on e_wider
              checkWiderSinks(e_wider)
            }
            case ExprSym(_) => () // Cannot get wider than a symbol, nothing to do here
            case _          => unreachable
          }

          checkWiderSinks(expr)
        }

      }

      case _ => ()
    }
    None
  }

}

object PortCheckB extends PairTransformerPass {
  val name = "port-check-b"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (cc.portCheckB(decl), cc.portCheckB(defn))
}
