////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// - Lower sram variables into sram instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.SramFactory
import com.argondesign.alogic.core.SyncRegFactory
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class SramBuilder {
  // Re-use SRAM entities of identical sizes
  private val store = mutable.Map[(Int, Int), (DeclEntity, DefnEntity)]()

  def srams: Iterable[(DeclEntity, DefnEntity)] = store.values

  def apply(width: Int, depth: Int)(implicit cc: CompilerContext): Symbol = synchronized {
    lazy val sram = SramFactory(s"sram_${depth}x$width", Loc.synthetic, width, depth)
    store.getOrElseUpdate((width, depth), sram)._1.symbol
  }

}

final class LowerSrams(
    sramBuilder: SramBuilder
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private val sep = cc.sep

  // Collection of various bits required to implement SRAMs
  // These are all the possible components required
  //  sSymbol: Symbol,        // The instance of the SRAM entity above
  //  rSymbol: Symbol,        // If the SRAM is of struct type, this is the local rdata signal
  //  oEntity: (Decl, Defn),  // If the output is registered, this is the SyncReg entity
  //  oSymbol: Symbol       // If the output is registered, the instance of the SyncReg entity above
  sealed private trait SramParts

  // TODO: sEntity is not unused in these

  // SRAM with wire driver and int/uint element type
  private case class SramWireInt(
      sSymbol: Symbol)
      extends SramParts

  // SRAM with wire driver and struct element type
  private case class SramWireStruct(
      sSymbol: Symbol,
      rSymbol: Symbol)
      extends SramParts

  // SRAM with reg driver and int/uint element type
  private case class SramRegInt(
      sSymbol: Symbol,
      oEntity: (DeclEntity, DefnEntity),
      oSymbol: Symbol)
      extends SramParts

  // SRAM with reg driver and struct element type
  private case class SramRegStruct(
      sSymbol: Symbol,
      rSymbol: Symbol,
      oEntity: (DeclEntity, DefnEntity),
      oSymbol: Symbol)
      extends SramParts

  // Extractors for transposed views of the above
  object SramWire {

    def unapply(parts: SramParts): Option[Symbol] = parts match {
      case SramWireInt(sSymbol)       => Some(sSymbol)
      case SramWireStruct(sSymbol, _) => Some(sSymbol)
      case _                          => None
    }

  }

  object SramReg {

    def unapply(parts: SramParts): Option[(Symbol, (DeclEntity, DefnEntity), Symbol)] =
      parts match {
        case SramRegInt(sSymbol, oEntity, oSymbol)       => Some((sSymbol, oEntity, oSymbol))
        case SramRegStruct(sSymbol, _, oEntity, oSymbol) => Some((sSymbol, oEntity, oSymbol))
        case _                                           => None
      }

  }

  object SramInt {

    def unapply(parts: SramParts): Option[Symbol] = parts match {
      case SramWireInt(sSymbol)      => Some(sSymbol)
      case SramRegInt(sSymbol, _, _) => Some(sSymbol)
      case _                         => None
    }

  }

  object SramStruct {

    def unapply(parts: SramParts): Option[(Symbol, Symbol)] = parts match {
      case SramWireStruct(sSymbol, rSymbol)      => Some((sSymbol, rSymbol))
      case SramRegStruct(sSymbol, rSymbol, _, _) => Some((sSymbol, rSymbol))
      case _                                     => None
    }

  }

  // Map from original sram variable symbol to the corresponding SramKit,
  private val sramMap = mutable.LinkedHashMap[Symbol, SramParts]()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case Decl(symbol) =>
        symbol.kind match {
          case TypeSram(kind, depth, st) =>
            val loc = tree.loc
            val name = symbol.name

            // Build the sram entity             // TODO: signed/unsigned
            val eSymbol = sramBuilder(kind.width.toInt, depth.toInt)

            // Create the instance
            val sSymbol = cc.newSymbol("sram" + sep + name, loc) tap {
              _.kind = eSymbol.kind.asType.kind
            }

            // If this is an SRAM with an underlying struct type, we need a new
            // signal to unpack the read data into, allocate this here
            lazy val rSymbol = cc.newSymbol(name + sep + "rdata", loc) tap { _.kind = kind }

            // If the sram is driven through registers, we need a SyncReg to drive it
            lazy val (oEntity, oSymbol) = {
              assert(st == StorageTypeReg)

              // Build the SyncReg entity
              val oEntity = {
                val oName = entitySymbol.name + sep + "or" + sep + name
                val weKind = sSymbol.kind.asEntity("we").get.kind.underlying
                val addrKind = sSymbol.kind.asEntity("addr").get.kind.underlying
                val weSymbol = cc.newSymbol("we", tree.loc) tap { _.kind = weKind }
                val addrSymbol = cc.newSymbol("addr", tree.loc) tap { _.kind = addrKind }
                val wdataSymbol = cc.newSymbol("wdata", tree.loc) tap { _.kind = kind }
                val oSymbol = cc.newSymbol(oName, tree.loc)
                val oKind = TypeType(TypeRecord(oSymbol, List(weSymbol, addrSymbol, wdataSymbol)))
                oSymbol.kind = oKind
                SyncRegFactory(oName, loc, oKind.kind)
              }

              // Create the instance
              val oSymbol = cc.newSymbol("or" + sep + name, loc) tap {
                _.kind = oEntity._1.symbol.kind.asType.kind
              }

              entitySymbol.attr.interconnectClearOnStall.append((oSymbol, s"ip${sep}valid"))

              (oEntity, oSymbol)
            }

            if (st == StorageTypeWire) {
              // Clear ce when the entity stalls
              entitySymbol.attr.interconnectClearOnStall.append((sSymbol, "ce"))
            }

            // We now have everything we possibly need, collect the relevant parts
            val sramParts = (kind, st) match {
              case (_: TypeInt, StorageTypeWire) =>
                SramWireInt(sSymbol)
              case (_: TypeRecord | _: TypeVector, StorageTypeWire) =>
                SramWireStruct(sSymbol, rSymbol)
              case (_: TypeInt, StorageTypeReg) =>
                SramRegInt(sSymbol, oEntity, oSymbol)
              case (_: TypeRecord | _: TypeVector, StorageTypeReg) =>
                SramRegStruct(sSymbol, rSymbol, oEntity, oSymbol)
              case _ =>
                cc.ice(tree, "Don't know how to build SRAM of type", kind.toSource)
            }

            sramMap(symbol) = sramParts

          //
          case _ =>
        }

      case _ =>
    }
    None
  }

  private def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))
  private def assignFalse(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 0))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSelect(ExprSym(symbol), "read", _), List(ArgP(addr)))) =>
        sramMap.get(symbol) map {
          case SramWire(iSymbol) =>
            val iRef = ExprSym(iSymbol)
            Thicket(
              List(
                assignTrue(iRef select "ce"),
                assignFalse(iRef select "we"),
                StmtAssign(iRef select "addr", addr)
              )
            )
          case SramReg(_, _, oSymbol) =>
            symbol.kind match {
              case TypeSram(kind, _, _) =>
                val oRef = ExprSym(oSymbol)
                val data = ExprInt(false, kind.width.toInt, 0) // Don't care
                Thicket(
                  List(
                    assignTrue(oRef select s"ip${sep}valid"),
                    StmtAssign(oRef select "ip", ExprCat(List(ExprInt(false, 1, 0), addr, data)))
                  )
                )
              case _ => unreachable
            }
        } getOrElse {
          tree
        }

      case StmtExpr(
            ExprCall(ExprSelect(ExprSym(symbol), "write", _), List(ArgP(addr), ArgP(data)))
          ) =>
        sramMap.get(symbol) map {
          case SramWire(iSymbol) =>
            val iRef = ExprSym(iSymbol)
            Thicket(
              List(
                assignTrue(iRef select "ce"),
                assignTrue(iRef select "we"),
                StmtAssign(iRef select "addr", addr),
                StmtAssign(iRef select "wdata", data)
              )
            )
          case SramReg(_, _, oSymbol) =>
            val oRef = ExprSym(oSymbol)
            Thicket(
              List(
                assignTrue(oRef select s"ip${sep}valid"),
                StmtAssign(oRef select "ip", ExprCat(List(ExprInt(false, 1, 1), addr, data)))
              )
            )
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprSelect(ExprSym(symbol), "rdata", _) =>
        sramMap.get(symbol) map {
          case SramInt(iSymbol)       => ExprSym(iSymbol) select "rdata"
          case SramStruct(_, rSymbol) => ExprSym(rSymbol)
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Replace Sram Decl/Defn with the Decl/Defn of the expanded symbols
      //////////////////////////////////////////////////////////////////////////

      case DeclSram(symbol, _, _, _) =>
        val newSymbols = sramMap(symbol) match {
          case SramWireInt(sSymbol)                        => List(sSymbol)
          case SramWireStruct(sSymbol, rSymbol)            => List(sSymbol, rSymbol)
          case SramRegInt(sSymbol, _, oSymbol)             => List(sSymbol, oSymbol)
          case SramRegStruct(sSymbol, rSymbol, _, oSymbol) => List(sSymbol, rSymbol, oSymbol)
        }
        Thicket(newSymbols map { _.mkDecl })

      case DefnSram(symbol) =>
        val newSymbols = sramMap(symbol) match {
          case SramWireInt(sSymbol)                        => List(sSymbol)
          case SramWireStruct(sSymbol, rSymbol)            => List(sSymbol, rSymbol)
          case SramRegInt(sSymbol, _, oSymbol)             => List(sSymbol, oSymbol)
          case SramRegStruct(sSymbol, rSymbol, _, oSymbol) => List(sSymbol, rSymbol, oSymbol)
        }
        Thicket(newSymbols map { _.mkDefn })

      //////////////////////////////////////////////////////////////////////////
      // Add sram connections
      //////////////////////////////////////////////////////////////////////////

      case defn: DefnEntity if sramMap.nonEmpty =>
        val connects = {
          // Add connects for read data unpacking
          sramMap.valuesIterator collect {
            case SramStruct(sS, rS) =>
              EntConnect(ExprSym(sS) select "rdata", List(ExprSym(rS)))
          }
        } concat {
          // Add connects for reg driver
          {
            sramMap.valuesIterator collect {
              case SramReg(sS, _, oS) =>
                Iterator(
                  EntConnect(ExprSym(oS) select s"op${sep}valid", List(ExprSym(sS) select "ce")),
                  EntConnect(
                    ExprSym(oS) select s"op",
                    List(ExprCat {
                      List(
                        ExprSym(sS) select "we",
                        ExprSym(sS) select "addr",
                        ExprSym(sS) select "wdata"
                      )
                    })
                  )
                )
            }
          }.flatten
        }

        defn.copy(body = List from (defn.body.iterator ++ connects))

      //
      case _ => tree
    }

    // If we did modify the node, regularize it
    if (result ne tree) {
      result regularize tree.loc
    }

    // Done
    result
  }

  override def finish(tree: Tree): Tree = tree match {
    // Note that we only collect register slices here,
    // SRAMs are added in the Pass dispatcher below
    case _: DeclEntity =>
      val extra = sramMap.valuesIterator collect { case SramReg(_, (oDecl, _), _) => oDecl }
      Thicket(tree :: extra.toList)
    case _: DefnEntity =>
      val extra = sramMap.valuesIterator collect { case SramReg(_, (_, oDefn), _) => oDefn }
      Thicket(tree :: extra.toList)
    case _ => unreachable
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit { case n @ ExprSelect(r, s, _) if r.tpe.isSram => cc.ice(n, s"SRAM .$s remains") }
  }

}

object LowerSrams {

  class LowerSramsPass extends PairTransformerPass {
    val name = "lower-srams"

    val sramBuilder = new SramBuilder

    def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
      (decl, defn) match {
        case (dcl: DeclEntity, dfn: DefnEntity) =>
          if (dcl.decls.isEmpty || dfn.variant == EntityVariant.Net) {
            // If no decls, or network, then there is nothing to do
            (decl, defn)
          } else {
            // Perform the transform
            val transformer = new LowerSrams(sramBuilder)
            // First transform the decl
            val newDecl = transformer(decl)
            // Then transform the defn
            val newDefn = transformer(defn)
            (newDecl, newDefn)
          }
        case _ => (decl, defn)
      }
    }

    override protected def finish(
        results: List[(Decl, Defn)]
      )(
        implicit
        cc: CompilerContext
      ): List[(Decl, Defn)] = {
      // Add all SRAMs to the results
      results ++ sramBuilder.srams
    }

  }

  def apply(): PairTransformerPass = new LowerSramsPass
}
