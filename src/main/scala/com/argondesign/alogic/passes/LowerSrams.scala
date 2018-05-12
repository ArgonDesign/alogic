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
// - Lower sram variables into sram instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.SramFactory
import com.argondesign.alogic.core.SyncRegFactory
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerSrams(implicit cc: CompilerContext) extends TreeTransformer {

  private val sep = cc.sep

  // Collection of various bits required to implement SRAMs
  // These are all the possible components required
  //  sEntity: Entity,      // The SRAM entity
  //  sSymbol: TermSymbol,  // The instance of the SRAM entity above
  //  rSymbol: TermSymbol,  // If the SRAM is of struct type, this is the local rdata signal
  //  oEntity: Entity],     // If the output is registered, this is the SyncReg entity
  //  oSymbol: TermSymbol   // If the output is registered, the instance of the SyncReg entity above
  private sealed trait SramParts

  // SRAM with wire driver and int/uint element type
  private case class SramWireInt(
      sEntity: Entity,
      sSymbol: TermSymbol
  ) extends SramParts
  // SRAM with wire driver and struct element type
  private case class SramWireStruct(
      sEntity: Entity,
      sSymbol: TermSymbol,
      rSymbol: TermSymbol
  ) extends SramParts
  // SRAM with reg driver and int/uint element type
  private case class SramRegInt(
      sEntity: Entity,
      sSymbol: TermSymbol,
      oEntity: Entity,
      oSymbol: TermSymbol
  ) extends SramParts
  // SRAM with reg driver and struct element type
  private case class SramRegStruct(
      sEntity: Entity,
      sSymbol: TermSymbol,
      rSymbol: TermSymbol,
      oEntity: Entity,
      oSymbol: TermSymbol
  ) extends SramParts

  // Extractors for transposed views of the above
  object SramWire {
    def unapply(parts: SramParts): Option[(Entity, TermSymbol)] = parts match {
      case SramWireInt(sEntity, sSymbol)       => Some((sEntity, sSymbol))
      case SramWireStruct(sEntity, sSymbol, _) => Some((sEntity, sSymbol))
      case _                                   => None
    }
  }

  object SramReg {
    def unapply(parts: SramParts): Option[(Entity, TermSymbol, Entity, TermSymbol)] = parts match {
      case SramRegInt(sEntity, sSymbol, oEntity, oSymbol) =>
        Some((sEntity, sSymbol, oEntity, oSymbol))
      case SramRegStruct(sEntity, sSymbol, _, oEntity, oSymbol) =>
        Some((sEntity, sSymbol, oEntity, oSymbol))
      case _ => None
    }
  }

  object SramInt {
    def unapply(parts: SramParts): Option[(Entity, TermSymbol)] = parts match {
      case SramWireInt(sEntity, sSymbol)      => Some((sEntity, sSymbol))
      case SramRegInt(sEntity, sSymbol, _, _) => Some((sEntity, sSymbol))
      case _                                  => None
    }
  }

  object SramStruct {
    def unapply(parts: SramParts): Option[(Entity, TermSymbol, TermSymbol)] = parts match {
      case SramWireStruct(sEntity, sSymbol, rSymbol)      => Some((sEntity, sSymbol, rSymbol))
      case SramRegStruct(sEntity, sSymbol, rSymbol, _, _) => Some((sEntity, sSymbol, rSymbol))
      case _                                              => None
    }
  }

  // Map from original sram variable symbol to the corresponding SramKit,
  private[this] val sramMap = mutable.LinkedHashMap[TermSymbol, SramParts]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.variant == "network"
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {

    case Decl(symbol, _) if symbol.kind.isSram => {
      val TypeSram(kind, depthExpr, st) = symbol.kind
      val loc = tree.loc
      val name = symbol.name

      // Build the sram entity
      val sEntity = {
        val eName = entitySymbol.name + sep + "sram" + sep + name
        val width = kind.width.value match {
          case Some(v) => v.toInt
          case None    => cc.fatal(symbol, "Width of SRAM is not a compile time constant")
        }
        val depth = depthExpr.value match {
          case Some(v) => v.toInt
          case None    => cc.fatal(symbol, "Depth of SRAM is not a compile time constant")
        }
        // TODO: reuse same shape entities
        // TODO: signed/unsigned
        SramFactory(eName, loc, width, depth)
      }

      // Create the instance
      val sSymbol = {
        val Sym(sEntitySymbol: TypeSymbol) = sEntity.ref
        cc.newTermSymbol("sram" + sep + name, loc, TypeInstance(sEntitySymbol))
      }

      // If this is an SRAM with an underlying struct type, we need a new
      // signal to unpack the read data into, allocate this here
      lazy val rSymbol = cc.newTermSymbol(name + sep + "rdata", loc, kind)

      // If the sram is driven through registers, we need a SyncReg to drive it
      lazy val (oEntity, oSymbol) = {
        assert(st == StorageTypeReg)

        // Build the SyncReg entity
        val oEntity = {
          val oName = entitySymbol.name + sep + "or" + sep + name
          val weKind = sSymbol.kind.asInstanceOf[TypeInstance]("we").get.underlying
          val addrKind = sSymbol.kind.asInstanceOf[TypeInstance]("addr").get.underlying
          val oKind = TypeStruct(oName, List("we", "addr", "wdata"), List(weKind, addrKind, kind))
          SyncRegFactory(oName, loc, oKind)
        }

        // Create the instance
        val oSymbol = {
          val Sym(oEntitySymbol: TypeSymbol) = oEntity.ref
          cc.newTermSymbol("or" + sep + name, loc, TypeInstance(oEntitySymbol))
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
          SramWireInt(sEntity, sSymbol)
        case (_: TypeStruct, StorageTypeWire) =>
          SramWireStruct(sEntity, sSymbol, rSymbol)
        case (_: TypeInt, StorageTypeReg) =>
          SramRegInt(sEntity, sSymbol, oEntity, oSymbol)
        case (_: TypeStruct, StorageTypeReg) =>
          SramRegStruct(sEntity, sSymbol, rSymbol, oEntity, oSymbol)
        case _ =>
          cc.ice(tree, "Don't know how to build SRAM of type", kind.toSource)
      }

      sramMap(symbol) = sramParts
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeReady
    ////////////////////////////////////////////////////////////////////////////

    case _: Stmt => {
      // Whenever we enter a new statement, add a new buffer to
      // store potential extra statements
      extraStmts.push(ListBuffer())
    }

    case _ =>
  }

  private[this] def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))
  private[this] def assignFalse(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 0))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "read"), List(addr))) => {
        sramMap.get(symbol) map {
          case SramWire(_, iSymbol) => {
            val iRef = ExprRef(iSymbol)
            StmtBlock(
              List(
                assignTrue(iRef select "ce"),
                assignFalse(iRef select "we"),
                StmtAssign(iRef select "addr", addr)
              ))
          }
          case SramReg(_, _, _, oSymbol) => {
            val oRef = ExprRef(oSymbol)
            val data = ExprInt(false, symbol.kind.width.value.get.toInt, 0) // Don't care
            StmtBlock(
              List(
                assignTrue(oRef select s"ip${sep}valid"),
                StmtAssign(oRef select "ip", ExprCat(List(ExprInt(false, 1, 0), addr, data)))
              ))
          }
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "write"), List(addr, data))) => {
        sramMap.get(symbol) map {
          case SramWire(_, iSymbol) => {
            val iRef = ExprRef(iSymbol)
            StmtBlock(
              List(
                assignTrue(iRef select "ce"),
                assignTrue(iRef select "we"),
                StmtAssign(iRef select "addr", addr),
                StmtAssign(iRef select "wdata", data)
              ))
          }
          case SramReg(_, _, _, oSymbol) => {
            val oRef = ExprRef(oSymbol)
            StmtBlock(
              List(
                assignTrue(oRef select s"ip${sep}valid"),
                StmtAssign(oRef select "ip", ExprCat(List(ExprInt(false, 1, 1), addr, data)))
              ))
          }
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprSelect(ExprRef(symbol: TermSymbol), "rdata") => {
        sramMap.get(symbol) map {
          case SramInt(_, iSymbol)       => ExprRef(iSymbol) select "rdata"
          case SramStruct(_, _, rSymbol) => ExprRef(rSymbol)
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add sram entities
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity if sramMap.nonEmpty => {
        // Drop sram, declarations, add rdata unpacking declarations
        val decls = {
          val newDecls = sramMap.valuesIterator collect {
            case SramStruct(_, _, rSymbol) => Decl(rSymbol, None)
          }
          val oldDecls = entity.declarations.iterator filter {
            case Decl(symbol, _) => !symbol.kind.isSram
            case _               => unreachable
          }
          newDecls ++ oldDecls
        }

        // Add instances
        val newInstances = sramMap.valuesIterator flatMap {
          case SramWire(sEntity, sSymbol) =>
            Iterator.single(Instance(Sym(sSymbol), sEntity.ref, Nil, Nil))
          case SramReg(sEntity, sSymbol, oEntity, oSymbol) =>
            Iterator(Instance(Sym(sSymbol), sEntity.ref, Nil, Nil),
                     Instance(Sym(oSymbol), oEntity.ref, Nil, Nil))
        }

        // Add connects for read data unpacking and reg driver
        val newConnects = {
          val unpackConnects = sramMap.valuesIterator collect {
            case SramStruct(_, sS, rS) => Connect(ExprRef(sS) select "rdata", List(ExprRef(rS)))
          }

          val driveConnect = sramMap.valuesIterator collect {
            case SramReg(_, sS, _, oS) =>
              Iterator(
                Connect(ExprRef(oS) select s"op${sep}valid", List(ExprRef(sS) select "ce")),
                Connect(ExprRef(oS) select s"op", List(ExprCat {
                  List(ExprRef(sS) select "we",
                       ExprRef(sS) select "addr",
                       ExprRef(sS) select "wdata")
                }))
              )
          }

          unpackConnects ++ driveConnect.flatten
        }

        val newEntity = entity.copy(
          declarations = decls.toList,
          instances = (newInstances ++ entity.instances).toList,
          connects = (newConnects ++ entity.connects).toList
        ) withVariant entity.variant

        val extraEntities = sramMap.valuesIterator flatMap {
          case SramWire(sEntity, _)            => Iterator.single(sEntity)
          case SramReg(sEntity, _, oEntity, _) => Iterator(sEntity, oEntity)
        }

        Thicket(newEntity :: extraEntities.toList)
      }

      case _ => tree
    }

    // Emit any extra statement with this statement
    val result2 = result match {
      case stmt: Stmt => {
        val extra = extraStmts.top
        if (extra.isEmpty) {
          stmt
        } else {
          extra append stmt
          StmtBlock(extra.toList)
        }
      } followedBy {
        extraStmts.pop()
      }
      case _ => result
    }

    // If we did modify the node, regularize it
    if (result2 ne tree) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    tree visit {
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isSram => {
        cc.ice(node, s"SRAM .${sel} remains")
      }
    }
  }

}

object LowerSrams extends TreeTransformerPass {
  val name = "lower-srams"
  def create(implicit cc: CompilerContext) = new LowerSrams
}
