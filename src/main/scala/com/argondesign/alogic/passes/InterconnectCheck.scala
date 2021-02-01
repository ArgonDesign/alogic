////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Check interconnections for  correctness, in particular:
//  - All bits are driven
//  - No multiple drivers
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.util.Ordinal
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec

object InterconnectCheck extends PairTransformerPass(parallel = true) {
  val name = "interconnect-check"

  private def check(assigns: List[EntAssign])(implicit cc: CompilerContext): Unit = {

    // A map from Either[symbol,(instance,port)] -> bit index -> list of driving assignments
    type DriverMap = Map[Either[Symbol, (Symbol, Symbol)], Map[Int, List[EntAssign]]]

    // Add a symbol bit range to a driver map
    def addRange(
        dmap: DriverMap,
        dst: Either[Symbol, (Symbol, Symbol)],
        lo: Int,
        width: Int,
        assign: EntAssign
      ): DriverMap = {
      require(lo >= 0)
      require(width >= 1)
      dmap.updatedWith(dst) { rmapOpt =>
        val someRmap = rmapOpt orElse Some {
          Map from { (0 until dst.fold(identity, _._2).kind.width.toInt).iterator map { _ -> Nil } }
        }
        someRmap map { rmap =>
          (lo until (lo + width)).iterator.foldLeft(rmap) {
            case (smap, bit) => smap.updatedWith(bit)(_ map { assign :: _ })
          }
        }
      }
    }

    // Build the driver map
    val driverMap: DriverMap = {
      def writtenParts(expr: Expr): Iterator[Expr] =
        expr match {
          case _: ExprSym            => Iterator.single(expr)
          case ExprCat(parts)        => parts.iterator flatMap writtenParts
          case _: ExprIndex          => Iterator.single(expr)
          case _: ExprSlice          => Iterator.single(expr)
          case InstancePortSel(_, _) => Iterator.single(expr)
          case _                     => unreachable // There should be no other ExprSel at this point
        }

      val empty: DriverMap = Map.empty

      def dst(expr: Expr): Either[Symbol, (Symbol, Symbol)] = expr match {
        case ExprSym(symbol)                   => Left(symbol)
        case InstancePortSel(iSymbol, pSymbol) => Right((iSymbol, pSymbol))
        case _                                 => unreachable
      }

      assigns.iterator
        .flatMap(assign => writtenParts(assign.lhs) map { (assign, _) })
        .foldLeft(empty) {
          case (dmap, (assign, expr)) =>
            expr match {
              case ExprIndex(tgt, idx) =>
                val iVal = idx.valueOption.get.toInt
                addRange(dmap, dst(tgt), iVal, 1, assign)
              case ExprSlice(tgt, lIdx, op, rIdx) =>
                val width = op match {
                  case ":" => (lIdx.valueOption.get - rIdx.valueOption.get).toInt + 1
                  case _   => rIdx.valueOption.get.toInt
                }
                val lo = op match {
                  case ":"  => rIdx.valueOption.get.toInt
                  case "+:" => lIdx.valueOption.get.toInt
                  case "-:" => lIdx.valueOption.get.toInt - width + 1
                }
                addRange(dmap, dst(tgt), lo, width, assign)
              case _ =>
                addRange(dmap, dst(expr), 0, expr.tpe.width.toInt, assign)
            }
        }
    }

    def ranges(bits: Iterable[Int]): String = {
      @tailrec
      def loop(
          acc: List[String],
          lo: Int,
          hi: Int,
          rest: List[Int]
        ): String = {
        def part: String = if (hi > lo) s"$hi:$lo" else lo.toString
        rest match {
          case Nil => (part :: acc) mkString ", "
          case head :: tail =>
            if (head == hi + 1) {
              loop(acc, lo, head, tail)
            } else {
              loop(part :: acc, head, head, tail)
            }
        }
      }
      bits.toList.sorted match {
        case head :: tail => loop(Nil, head, head, tail)
        case _            => unreachable
      }
    }

    val messages = driverMap flatMap {
      case (dst, rmap) =>
        // Group by drivers, keep bit indices, if not one driver, it's bad
        val bad = rmap.groupMap(_._2)(_._1).iterator filter { _._1.sizeIs != 1 }
        Option.when(bad.hasNext) {
          dst -> {
            val what = dst.fold(_.name, { case (i, p) => s"${i.name}.${p.name}" })
            val where = dst.fold(identity, _._1).loc
            bad.toSeq sortBy {
              case (assigns, bits) => (-assigns.length, bits.min)
            } map {
              case (assigns, bits) => (assigns.sortBy(_.loc), ranges(bits))
            } flatMap {
              case (Nil, ranges) =>
                Iterator.single(Error(where, s"Bits $ranges of '$what' are undriven"))
              case (assigns, ranges) =>
                Iterator.single(
                  Error(where, s"Bits $ranges of '$what' have multiple drivers")
                ) concat {
                  assigns.iterator.zipWithIndex map {
                    case (assign, idx) => Note(assign, s"The ${Ordinal(idx + 1)} driver is here")
                  }
                }
            }
          }
        }
    }

    messages.toSeq sortBy {
      _._1.fold(s => (s.loc, s.loc), { case (i, p) => (i.loc, p.loc) })
    } foreach {
      _._2 foreach cc.addMessage
    }
  }

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    defn match {
      case d: DefnEntity => check(d.assigns)
      case _             =>
    }
    (decl, defn)
  }

}
