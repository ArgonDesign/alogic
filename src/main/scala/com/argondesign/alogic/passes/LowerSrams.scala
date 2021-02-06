////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// - Lower SRAM variables into SRAM instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class LowerSrams(implicit cc: CompilerContext) extends StatefulTreeTransformer {
  private val sep = cc.sep

  // Collection of various bits required to implement SRAMs
  // These are all the possible components required
  //  sSymbol: Symbol,  // The instance of the SRAM entity above
  //  rSymbol: Symbol,  // If the SRAM is of struct type, this is the local rdata signal
  //  oSymbol: Symbol   // If the output is registered, the instance of the SyncReg entity above
  sealed private trait SramParts

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
      oSymbol: Symbol)
      extends SramParts

  // SRAM with reg driver and struct element type
  private case class SramRegStruct(
      sSymbol: Symbol,
      rSymbol: Symbol,
      oSymbol: Symbol)
      extends SramParts

  // Extractors for transposed views of the above
  private object SramWire {

    def unapply(parts: SramParts): Option[Symbol] = parts match {
      case SramWireInt(sSymbol)       => Some(sSymbol)
      case SramWireStruct(sSymbol, _) => Some(sSymbol)
      case _                          => None
    }

  }

  private object SramReg {

    def unapply(parts: SramParts): Option[(Symbol, Symbol)] =
      parts match {
        case SramRegInt(sSymbol, oSymbol)       => Some((sSymbol, oSymbol))
        case SramRegStruct(sSymbol, _, oSymbol) => Some((sSymbol, oSymbol))
        case _                                  => None
      }

  }

  private object SramInt {

    def unapply(parts: SramParts): Option[Symbol] = parts match {
      case SramWireInt(sSymbol)   => Some(sSymbol)
      case SramRegInt(sSymbol, _) => Some(sSymbol)
      case _                      => None
    }

  }

  private object SramStruct {

    def unapply(parts: SramParts): Option[(Symbol, Symbol)] = parts match {
      case SramWireStruct(sSymbol, rSymbol)   => Some((sSymbol, rSymbol))
      case SramRegStruct(sSymbol, rSymbol, _) => Some((sSymbol, rSymbol))
      case _                                  => None
    }

  }

  // Map from original sram variable symbol to the corresponding SramParts,
  private val sramMap = mutable.LinkedHashMap[Symbol, SramParts]()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case Decl(symbol) =>
        symbol.kind match {
          case TypeSram(kind, depth, st) =>
            val loc = tree.loc
            val name = symbol.name

            // Build the sram entity             // TODO: signed/unsigned
            val eSymbol = cc.sramFactory(depth.toInt, kind.width.toInt)

            // Create the instance
            val sSymbol = Symbol("sram" + sep + name, loc) tap {
              _.kind = eSymbol.kind.asType.kind
            }

            // If this is an SRAM with an underlying struct type, we need a new
            // signal to unpack the read data into, allocate this here
            lazy val rSymbol = Symbol(name + sep + "rdata", loc) tap { _.kind = kind }

            // If the sram is driven through registers, we need a SyncReg to drive it
            lazy val oSymbol = {
              assert(st == StorageTypeReg)

              // Build the SyncReg entity
              val oEntitySymbol = {
                val oName = entitySymbol.name + sep + "or" + sep + name
                val weKind = sSymbol.kind.asEntity("we").get.kind.underlying
                val addrKind = sSymbol.kind.asEntity("addr").get.kind.underlying
                val weSymbol = Symbol("we", tree.loc) tap { _.kind = weKind }
                val addrSymbol = Symbol("addr", tree.loc) tap { _.kind = addrKind }
                val wdataSymbol = Symbol("wdata", tree.loc) tap { _.kind = kind }
                val oSymbol = Symbol(oName, tree.loc)
                oSymbol.scopeName = entitySymbol.scopeName
                val oKind = TypeType(TypeRecord(oSymbol, List(weSymbol, addrSymbol, wdataSymbol)))
                oSymbol.kind = oKind
                cc.syncRegFactory(oKind.kind)
              }

              // Create the instance
              val oSymbol = Symbol("or" + sep + name, loc) tap {
                _.kind = oEntitySymbol.kind.asType.kind
              }

              entitySymbol.attr.interconnectClearOnStall.append((oSymbol, s"i_valid"))

              oSymbol
            }

            if (st == StorageTypeWire) {
              // Clear ce when the entity stalls
              entitySymbol.attr.interconnectClearOnStall.append((sSymbol, "ce"))
            }

            // We now have everything we possibly need, collect the relevant parts
            sramMap(symbol) = (kind, st) match {
              case (_: TypeInt, StorageTypeWire) =>
                SramWireInt(sSymbol)
              case (_: TypeRecord | _: TypeVector, StorageTypeWire) =>
                SramWireStruct(sSymbol, rSymbol)
              case (_: TypeInt, StorageTypeReg) =>
                SramRegInt(sSymbol, oSymbol)
              case (_: TypeRecord | _: TypeVector, StorageTypeReg) =>
                SramRegStruct(sSymbol, rSymbol, oSymbol)
              case _ =>
                throw Ice(tree, "Don't know how to build SRAM of type", kind.toSource)
            }

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

      case StmtExpr(ExprCall(ExprSel(ExprSym(symbol), "read"), List(ArgP(addr)))) =>
        sramMap.get(symbol) map {
          case SramWire(iSymbol) =>
            val iRef = ExprSym(iSymbol)
            Thicket(
              List(
                assignTrue(iRef sel "ce"),
                assignFalse(iRef sel "we"),
                StmtAssign(iRef sel "addr", addr)
              )
            )
          case SramReg(_, oSymbol) =>
            symbol.kind match {
              case TypeSram(kind, _, _) =>
                val oRef = ExprSym(oSymbol)
                val data = ExprInt(false, kind.width.toInt, 0) // Don't care
                Thicket(
                  List(
                    assignTrue(oRef sel s"i_valid"),
                    StmtAssign(
                      oRef sel "i_payload",
                      ExprCat(List(ExprInt(false, 1, 0), addr, data))
                    )
                  )
                )
              case _ => unreachable
            }
          case _ => unreachable // Above covers all
        } getOrElse {
          tree
        }

      case StmtExpr(
            ExprCall(ExprSel(ExprSym(symbol), "write"), List(ArgP(addr), ArgP(data)))
          ) =>
        sramMap.get(symbol) map {
          case SramWire(iSymbol) =>
            val iRef = ExprSym(iSymbol)
            Thicket(
              List(
                assignTrue(iRef sel "ce"),
                assignTrue(iRef sel "we"),
                StmtAssign(iRef sel "addr", addr),
                StmtAssign(iRef sel "wdata", data)
              )
            )
          case SramReg(_, oSymbol) =>
            val oRef = ExprSym(oSymbol)
            Thicket(
              List(
                assignTrue(oRef sel "i_valid"),
                StmtAssign(oRef sel "i_payload", ExprCat(List(ExprInt(false, 1, 1), addr, data)))
              )
            )
          case _ => unreachable // Above covers all
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprSel(ExprSym(symbol), "rdata") =>
        sramMap.get(symbol) map {
          case SramInt(iSymbol)       => ExprSym(iSymbol) sel "rdata"
          case SramStruct(_, rSymbol) => ExprSym(rSymbol)
          case _                      => unreachable // Above covers all
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Replace Sram Decl/Defn with the Decl/Defn of the expanded symbols
      //////////////////////////////////////////////////////////////////////////

      case DeclSram(symbol, _, _, _) =>
        val newSymbols = sramMap(symbol) match {
          case SramWireInt(sSymbol)                     => List(sSymbol)
          case SramWireStruct(sSymbol, rSymbol)         => List(sSymbol, rSymbol)
          case SramRegInt(sSymbol, oSymbol)             => List(sSymbol, oSymbol)
          case SramRegStruct(sSymbol, rSymbol, oSymbol) => List(sSymbol, rSymbol, oSymbol)
        }
        Thicket(newSymbols map { _.mkDecl })

      case DefnSram(symbol) =>
        val newSymbols = sramMap(symbol) match {
          case SramWireInt(sSymbol)                     => List(sSymbol)
          case SramWireStruct(sSymbol, rSymbol)         => List(sSymbol, rSymbol)
          case SramRegInt(sSymbol, oSymbol)             => List(sSymbol, oSymbol)
          case SramRegStruct(sSymbol, rSymbol, oSymbol) => List(sSymbol, rSymbol, oSymbol)
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
              EntAssign(ExprSym(rS), ExprSym(sS) sel "rdata")
          }
        } concat {
          // Add connects for reg driver
          {
            sramMap.valuesIterator collect {
              case SramReg(sS, oS) =>
                Iterator(
                  EntAssign(ExprSym(sS) sel "ce", ExprSym(oS) sel "o_valid"),
                  EntAssign(
                    ExprCat(
                      List(
                        ExprSym(sS) sel "we",
                        ExprSym(sS) sel "addr",
                        ExprSym(sS) sel "wdata"
                      )
                    ),
                    ExprSym(oS) sel "o_payload"
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

  override def finalCheck(tree: Tree): Unit = {
    tree visit { case n @ ExprSel(r, s) if r.tpe.isSram => throw Ice(n, s"SRAM .$s remains") }
  }

}

object LowerSrams extends EntityTransformerPass(declFirst = true) {
  val name = "lower-srams"

  override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = defn.variant == EntityVariant.Net

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new LowerSrams
}
