////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Add the clock and reset signals to all entities and wire them through
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

final class AddClockAndResetA(
    globalReplacements: mutable.Map[Symbol, Symbol]
)(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  private val clk: Symbol = cc.newSymbol("clk", Loc.synthetic)
  clk.kind = TypeIn(TypeUInt(1), FlowControlTypeNone)
  clk.attr.clk set true
  private val rst: Symbol = cc.newSymbol(cc.rst, Loc.synthetic)
  rst.kind = TypeIn(TypeUInt(1), FlowControlTypeNone)
  rst.attr.rst set true

  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeType(_: TypeEntity) => true
    case _                       => false
  }

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: DeclEntity | _: DefnEntity => false
    case _                             => true
  }

  override protected def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Add clock and reset ports to all entities
    ////////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity =>
      globalReplacements(orig(decl.symbol)) = decl.symbol
      val clkDecl = clk.mkDecl regularize Loc.synthetic
      val rstDecl = rst.mkDecl regularize Loc.synthetic
      TypeAssigner(decl.copy(decls = clkDecl :: rstDecl :: decl.decls) withLoc tree.loc)

    case defn: DefnEntity =>
      val clkDefn = EntDefn(clk.mkDefn) regularize Loc.synthetic
      val rstDefn = EntDefn(rst.mkDefn) regularize Loc.synthetic
      TypeAssigner(defn.copy(body = clkDefn :: rstDefn :: defn.body) withLoc tree.loc)

    case _ => tree
  }
}

final class AddClockAndResetB(
    globalReplacements: collection.Map[Symbol, Symbol]
)(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  override protected def skip(tree: Tree): Boolean = tree match {
    case _ => false
  }

  override protected def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Update instance types
    ////////////////////////////////////////////////////////////////////////////

    case decl @ DeclInstance(_, ExprSym(eSymbol)) =>
      globalReplacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    ////////////////////////////////////////////////////////////////////////////
    // Wire clock and reset ports through to instances
    ////////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity =>
      val clk = defn.defns.head.symbol
      val rst = defn.defns(1).symbol

      val newBody = List from {
        defn.body.iterator concat {
          defn.instances flatMap {
            case Defn(iSymbol) =>
              val clkConn = EntConnect(ExprSym(clk), List(ExprSym(iSymbol) select "clk")) regularize Loc.synthetic
              val rstConn = EntConnect(ExprSym(rst), List(ExprSym(iSymbol) select cc.rst)) regularize Loc.synthetic
              Iterator(clkConn, rstConn)
          }
        }
      }

      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case _ => tree
  }

  override def finish(tree: Tree): Tree = tree tap {
    case defn: DefnEntity =>
      val icos = defn.symbol.attr.interconnectClearOnStall.getOrElse(Nil)
      defn.symbol.attr.interconnectClearOnStall set {
        icos map {
          case (symbol, name) => (repl(symbol).getOrElse(symbol), name)
        }
      }
    case _ =>
  }
}

object AddClockAndReset {

  def apply(): Pass[List[(Decl, Defn)], List[(Decl, Defn)]] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()

    new EntityTransformerPass(declFirst = true) {
      val name = "add-clock-and-reset-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new AddClockAndResetA(globalReplacements)
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "add-clock-and-reset-b"

      override protected def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
        super.skip(decl, defn) || decl.asInstanceOf[DeclEntity].instances.isEmpty

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new AddClockAndResetB(globalReplacements)
    }
  }
}
