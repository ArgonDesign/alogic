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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerSrams(implicit cc: CompilerContext) extends TreeTransformer {

  // Map from original sram variable symbol to the
  // corresponding sram entity and instance symbols
  private[this] val sramMap = mutable.Map[TermSymbol, (Entity, TermSymbol)]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.variant == "network"
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {

    case Decl(symbol, _) if symbol.kind.isSram => {
      // Construct the sram entity
      val TypeSram(kind, depth, st) = symbol.kind
      // TODO: handle registered output
      // TODO: always non-struct data ports
      val loc = tree.loc
      val sName = symbol.name
      val eName = entitySymbol.name + cc.sep + "sram" + cc.sep + sName
      val sramEntity: Entity = SramFactory(eName, loc, kind, depth)
      val Sym(sramEntitySymbol: TypeSymbol) = sramEntity.ref
      val instanceSymbol = {
        cc.newTermSymbol(sName, loc, TypeInstance(sramEntitySymbol))
      }
      sramMap(symbol) = (sramEntity, instanceSymbol)
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
          case (_, iSymbol) => {
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
          case (_, iSymbol) => {
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
          case (_, iSymbol) => ExprRef(iSymbol) select "rdata"
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add sram entities
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity if sramMap.nonEmpty => {
        // Drop sram, declarations
        val declarations = entity.declarations filterNot {
          case Decl(symbol, _) => symbol.kind.isSram
          case _               => false
        }

        // Add instances
        val instances = for ((entity, instance) <- sramMap.values) yield {
          Instance(Sym(instance), entity.ref, Nil, Nil)
        }

        // Add fence statements
        val fenceStmts = sramMap.values map { _._2 } map { iSymbol =>
          assignFalse(ExprRef(iSymbol) select "ce")
        }

        val newEntity = entity.copy(
          declarations = declarations,
          instances = instances.toList ::: entity.instances,
          fenceStmts = fenceStmts.toList ::: entity.fenceStmts
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
