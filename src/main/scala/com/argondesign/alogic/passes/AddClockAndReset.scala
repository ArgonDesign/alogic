////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Add the clock and reset signals to all entities and wire them through
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._

import scala.collection.concurrent.TrieMap

final class AddClockAndResetA(
    globalReplacements: TrieMap[Symbol, Symbol]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private val clk: Symbol = Symbol("clk", Loc.synthetic)
  clk.kind = TypeIn(TypeUInt(1), FlowControlTypeNone)
  clk.attr.clk set true
  private val rst: Symbol = Symbol(cc.rst, Loc.synthetic)
  rst.kind = TypeIn(TypeUInt(1), FlowControlTypeNone)
  rst.attr.rst set true

  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeType(_: TypeEntity) => true
    case _                       => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    case _: DeclEntity | _: DefnEntity => None
    case _                             => Some(tree)
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
      val clkDefn = EntSplice(clk.mkDefn) regularize Loc.synthetic
      val rstDefn = EntSplice(rst.mkDefn) regularize Loc.synthetic
      TypeAssigner(defn.copy(body = clkDefn :: rstDefn :: defn.body) withLoc tree.loc)

    case _ => tree
  }

}

final class AddClockAndResetB(
    globalReplacements: collection.Map[Symbol, Symbol]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
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
              val clkAssign = EntAssign(
                ExprSym(iSymbol) sel "clk",
                ExprSym(clk)
              ) regularize Loc.synthetic
              val rstAssign = EntAssign(
                ExprSym(iSymbol) sel cc.rst,
                ExprSym(rst)
              ) regularize Loc.synthetic
              Iterator(clkAssign, rstAssign)
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

  def apply(): Pass[Pairs, Pairs] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()

    new EntityTransformerPass(declFirst = true, parallel = true) {
      val name = "add-clock-and-reset-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new AddClockAndResetA(globalReplacements)
    } andThen new EntityTransformerPass(declFirst = true, parallel = true) {
      val name = "add-clock-and-reset-b"

      override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = decl.instances.isEmpty

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new AddClockAndResetB(globalReplacements)
    }
  }

}
