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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.SramFactory
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerSrams(implicit cc: CompilerContext) extends TreeTransformer {

  // Map from original sram variable symbol to the corresponding sram entity,
  // instance symbols, and optional rdata symbol if this is a struct sram
  private[this] val sramMap = mutable.Map[TermSymbol, (Entity, TermSymbol, Option[TermSymbol])]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.variant == "network"
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {

    case Decl(symbol, _) if symbol.kind.isSram => {
      // TODO: handle registered output
      val TypeSram(kind, depthExpr, st) = symbol.kind
      val loc = tree.loc
      val sName = symbol.name

      // Build the sram entity
      val eName = entitySymbol.name + cc.sep + "sram" + cc.sep + sName
      val width = kind.width.value match {
        case Some(v) => v.toInt
        case None    => cc.fatal(symbol, "Width of SRAM is not a compile time constant")
      }
      val depth = depthExpr.value match {
        case Some(v) => v.toInt
        case None    => cc.fatal(symbol, "Depth of SRAM is not a compile time constant")
      }
      val sramEntity: Entity = SramFactory(eName, loc, width, depth)

      // Create the instance
      val instanceSymbol = {
        val Sym(sramEntitySymbol: TypeSymbol) = sramEntity.ref
        cc.newTermSymbol(sName, loc, TypeInstance(sramEntitySymbol))
      }

      // If this is an SRAM with an underlying struct type, we need a new
      // signal to unpack the read data into, allocate this here
      val rdataOpt = kind match {
        case _: TypeInt    => None
        case _: TypeStruct => Some(cc.newTermSymbol(sName + cc.sep + "rdata", loc, kind))
        case _             => cc.ice("Don't know how to build SRAM with data type", kind.toSource)
      }

      // We now have everything we need
      sramMap(symbol) = (sramEntity, instanceSymbol, rdataOpt)

      // Clear ce when the entity stalls
      entitySymbol.attr.interconnectClearOnStall.append((instanceSymbol, "ce"))
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
          case (_, iSymbol, _) => {
            val iRef = ExprRef(iSymbol)
            StmtBlock(
              List(
                assignTrue(iRef select "ce"),
                assignFalse(iRef select "we"),
                StmtAssign(iRef select "addr", addr)
              ))
          }
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "write"), List(addr, data))) => {
        sramMap.get(symbol) map {
          case (_, iSymbol, _) => {
            val iRef = ExprRef(iSymbol)
            StmtBlock(
              List(
                assignTrue(iRef select "ce"),
                assignTrue(iRef select "we"),
                StmtAssign(iRef select "addr", addr),
                StmtAssign(iRef select "wdata", data)
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
          case (_, iSymbol, None)   => ExprRef(iSymbol) select "rdata"
          case (_, _, Some(symbol)) => ExprRef(symbol)
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
            case (_, _, Some(symbol)) => Decl(symbol, None)
          }
          val oldDecls = entity.declarations.iterator filter {
            case Decl(symbol, _) => !symbol.kind.isSram
            case _               => unreachable
          }
          newDecls ++ oldDecls
        }

        // Add instances
        val newInstances = for ((entity, instance, _) <- sramMap.valuesIterator) yield {
          Instance(Sym(instance), entity.ref, Nil, Nil)
        }

        // Add fence statements
        val newFenceStmts = sramMap.valuesIterator map { _._2 } map { iSymbol =>
          assignFalse(ExprRef(iSymbol) select "ce")
        }

        // Add read data unpacking connects for struct srams
        val newConnects = sramMap.valuesIterator collect {
          case (_, iSymbol, Some(rdSymbol)) =>
            Connect(ExprRef(iSymbol) select "rdata", List(ExprRef(rdSymbol)))
        }

        val newEntity = entity.copy(
          declarations = decls.toList,
          instances = (newInstances ++ entity.instances).toList,
          connects = (newConnects ++ entity.connects).toList,
          fenceStmts = (newFenceStmts ++ entity.fenceStmts).toList
        ) withVariant entity.variant

        val sramEntities = sramMap.values map { _._1 }

        Thicket(newEntity :: sramEntities.toList)
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
