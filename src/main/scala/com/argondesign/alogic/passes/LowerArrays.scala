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
// Lower arrays into constituent signals
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.lib.Math

import scala.collection.mutable

final class LowerArrays(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // List of array we/waddr/wdata symbols
  private val arrays = mutable.ListBuffer[(Symbol, Symbol, Symbol)]()

  // The clock symbol
  private var clk: Symbol = _

  override protected def start(tree: Tree): Unit = tree match {
    case decl: DeclEntity => clk = decl.clk.get
    case _                =>
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case Decl(symbol) =>
        symbol.kind match {
          case TypeArray(kind, size) =>
            val loc = tree.loc
            val name = symbol.name
            // Create we symbol
            val weSymbol = cc.newSymbol(s"${name}_we", loc) tap { _.kind = TypeUInt(1) }
            weSymbol.attr.clearOnStall set true
            weSymbol.attr.combSignal set true
            // Create waddr symbol
            val abits = Math.clog2(size) ensuring { _ > 0 }
            val waSymbol = cc.newSymbol(s"${name}_waddr", loc) tap { _.kind = TypeUInt(abits) }
            waSymbol.attr.combSignal set true
            // Create wdata symbol
            val dbits = kind.width
            val wdSymbol = cc.newSymbol(s"${name}_wdata", loc) tap { _.kind = TypeUInt(dbits) }
            wdSymbol.attr.combSignal set true
            // Memorize
            symbol.attr.memory.set((weSymbol, waSymbol, wdSymbol))
            arrays.append((weSymbol, waSymbol, wdSymbol))

          case _ =>
        }

      case defn: DefnEntity => assert(defn.combProcesses.lengthIs <= 1, defn.symbol.name)

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite .write calls
    //////////////////////////////////////////////////////////////////////////

    case StmtExpr(
          ExprCall(ExprSel(ExprSym(symbol), "write"), List(ArgP(addr), ArgP(data)))
        ) =>
      // Rewrite assignments to array elements
      symbol.attr.memory.get map {
        case (weSymbol, waSymbol, wdSymbol) =>
          Thicket(
            List(
              StmtAssign(ExprSym(weSymbol), ExprInt(false, 1, 1)),
              StmtAssign(ExprSym(waSymbol), addr),
              StmtAssign(ExprSym(wdSymbol), data)
            )
          ) regularize tree.loc
      } getOrElse {
        tree
      }

    //////////////////////////////////////////////////////////////////////////
    // Add extra Decl/Defn and the clocked process
    //////////////////////////////////////////////////////////////////////////

    case DeclArray(symbol, _, _) =>
      val (we, wa, wd) = symbol.attr.memory.value
      Thicket(List(tree, we.mkDecl, wa.mkDecl, wd.mkDecl)) regularize tree.loc

    case EntSplice(DefnArray(symbol)) =>
      val (we, wa, wd) = symbol.attr.memory.value
      val stmts = List(
        StmtComment(s"Distributed memory '${symbol.name}' - line ${symbol.loc.line}"),
        StmtIf(
          ExprSym(we),
          List(StmtDelayed(ExprSym(symbol) index ExprSym(wa), ExprSym(wd))),
          Nil
        )
      )
      // Append _q to array name symbol
      symbol.name = s"${symbol.name}_q"
      Thicket(
        List(
          tree,
          EntSplice(we.mkDefn),
          EntSplice(wa.mkDefn),
          EntSplice(wd.mkDefn),
          EntClockedProcess(ExprSym(clk), None, stmts)
        )
      ) regularize tree.loc

    //////////////////////////////////////////////////////////////////////////
    // Add _we/_waddr/_wdata = 'b0 fence statements
    //////////////////////////////////////////////////////////////////////////

    case EntCombProcess(stmts) =>
      val newBody = List from {
        arrays.iterator map {
          case (weSymbol, waSymbol, wdSymbol) =>
            StmtBlock(
              List(
                StmtAssign(ExprSym(weSymbol), ExprInt(false, 1, 0)),
                StmtAssign(ExprSym(waSymbol), ExprInt(false, waSymbol.kind.width.toInt, 0)),
                StmtAssign(ExprSym(wdSymbol), ExprInt(false, wdSymbol.kind.width.toInt, 0))
              )
            ) regularize weSymbol.loc
        } concat stmts
      }
      arrays.clear()
      TypeAssigner(EntCombProcess(newBody) withLoc tree.loc)

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = tree match {
    case _: DeclEntity =>
    case _             => assert(arrays.isEmpty)
  }

}

object LowerArrays extends EntityTransformerPass(declFirst = true, parallel = true) {
  val name = "lower-arrays"

  override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = defn.variant != EntityVariant.Fsm

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerArrays
}
