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
// Lower variables into flops and combinatorial nets. At this stage, anything
// that is of TypeInt is a flop, unless it is driven through a connect, or is
// one of the control signals of a memory, in which case it is a net.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.enums.ResetStyle

import scala.collection.mutable.ListBuffer

final class LowerVariables(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private val resetFlops = new ListBuffer[(Symbol, Symbol, Expr)]()
  private val nonResetFlops = new ListBuffer[(Symbol, Symbol)]()

  // The clock and reset symbols
  private var clk: Symbol = _
  private var rst: Symbol = _

  override protected def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      clk = defn.clk.get
      rst = defn.rst.get
    case _ =>
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    case defn: DefnEntity =>
      // Drop the oreg prefix from the flops allocated for registered outputs,
      // These will now gain _d and _q, so the names will become unique.
      val prefix = s"`oreg${cc.sep}"
      val prefixLen = prefix.length
      for {
        Defn(symbol) <- defn.defns
        if symbol.name startsWith prefix
      } {
        symbol.name = symbol.name drop prefixLen
      }

      // Mark local symbols driven by Connect as combinational nets
      for {
        EntAssign(lhs, _) <- defn.assigns
        symbol <- WrittenSymbols(lhs)
        if symbol.kind.isInt
      } {
        symbol.attr.combSignal set true
      }

      defn.defns foreach {
        case defn @ DefnVar(symbol, initOpt) if !(symbol.attr.combSignal contains true) =>
          val loc = defn.loc
          val name = symbol.name
          // Append _q to the name of the symbol
          symbol.name = s"${name}_q"
          // Create the _d symbol
          val dSymbol = cc.newSymbol(s"${name}_d", loc) tap {
            _.kind = symbol.kind
          }
          // Move the clearOnStall attribute to the _d symbol
          symbol.attr.clearOnStall.get foreach { attr =>
            dSymbol.attr.clearOnStall set attr
            symbol.attr.clearOnStall.clear()
          }
          // If the symbol has a default attribute, move that to the _d,
          // otherwise use the _q as the default initializer
          val default = symbol.attr.default.getOrElse {
            ExprSym(symbol) regularize loc
          }
          dSymbol.attr.default set default
          symbol.attr.default.clear()
          // Mark _d as tmp if _q is tmp
          if (symbol.attr.tmp.isSet) {
            dSymbol.attr.tmp set true
          }
          // Set attributes
          symbol.attr.flop set dSymbol
          // Memorize
          if (cc.settings.resetAll || initOpt.isDefined) {
            val kind = symbol.kind
            val resetExpr = initOpt getOrElse ExprInt(kind.isSigned, kind.width.toInt, 0)
            resetFlops.append((symbol, dSymbol, resetExpr))
          } else {
            nonResetFlops.append((symbol, dSymbol))
          }

        case _ =>
      }
      None

    // We do not replace _q with _d in connects, connects always use the _q
    case _: EntAssign => Some(tree)

    // Do not replace _q with _d below ExprOld
    case ExprOld(expr) =>
      if (expr collect { case ExprSym(symbol) => symbol } exists { !_.attr.flop.isSet }) {
        throw Ice(tree, "Non-flop symbol under ExprOld")
      }
      Some(expr)

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite references
    //////////////////////////////////////////////////////////////////////////

    case ExprSym(qSymbol) =>
      // Rewrite references to flops as references to the _d,
      qSymbol.attr.flop.get map { dSymbol =>
        ExprSym(dSymbol) regularize tree.loc
      } getOrElse {
        tree
      }

    //////////////////////////////////////////////////////////////////////////
    // Add the clocked processes
    //////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity =>
      val resetProcess = Option.when(resetFlops.nonEmpty) {
        val rstLow = cc.settings.resetStyle match {
          case ResetStyle.AsyncLow | ResetStyle.SyncLow => true
          case _                                        => false
        }
        val resetAssigns = List from {
          resetFlops.iterator map {
            case (qSymbol, _, resetVal) => StmtDelayed(ExprSym(qSymbol), resetVal)
          }
        }
        val dAssigns = {
          val assigns = List from {
            resetFlops.iterator map {
              case (qSymbol, dSymbol, _) => StmtDelayed(ExprSym(qSymbol), ExprSym(dSymbol))
            }
          }
          defn.go match {
            case None => assigns
            case Some(goSymbol) =>
              List(StmtIf(ExprSym(goSymbol), assigns, Nil))
          }
        }
        val resetRefOpt = cc.settings.resetStyle match {
          case ResetStyle.AsyncHigh | ResetStyle.AsyncLow => Some(ExprSym(rst))
          case _                                          => None
        }
        EntClockedProcess(
          ExprSym(clk),
          resetRefOpt,
          List(
            StmtComment("Reset flops"),
            StmtIf(
              if (rstLow) !ExprSym(rst) else ExprSym(rst),
              resetAssigns,
              dAssigns
            )
          )
        ) regularize tree.loc
      }
      val nonResetProcess = Option.when(nonResetFlops.nonEmpty) {
        val dAssigns = {
          val assigns = List from {
            nonResetFlops.iterator map {
              case (qSymbol, dSymbol) => StmtDelayed(ExprSym(qSymbol), ExprSym(dSymbol))
            }
          }
          defn.go match {
            case None => assigns
            case Some(goSymbol) =>
              List(StmtIf(ExprSym(goSymbol), assigns, Nil))
          }
        }
        EntClockedProcess(
          ExprSym(clk),
          None,
          StmtComment("Non-reset flops") :: dAssigns
        ) regularize tree.loc
      }
      if (resetProcess.isDefined || nonResetProcess.isDefined) {
        val newBody = defn.body ++ resetProcess.iterator ++ nonResetProcess.iterator
        TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)
      } else {
        tree
      }

    //////////////////////////////////////////////////////////////////////////
    // Add declarations and definitions
    //////////////////////////////////////////////////////////////////////////

    case decl @ Decl(qSymbol) =>
      qSymbol.attr.flop.get map { dSymbol =>
        Thicket(List(decl, dSymbol.mkDecl)) regularize tree.loc
      } getOrElse tree

    case defn @ Defn(qSymbol) =>
      qSymbol.attr.flop.get map { dSymbol =>
        Thicket(List(defn, dSymbol.mkDefn)) regularize tree.loc
      } getOrElse tree

    //////////////////////////////////////////////////////////////////////////
    // Note: Initial _d = _q fence statements will be added in
    // DefaultAssignments as required
    //////////////////////////////////////////////////////////////////////////

    case _ => tree
  }

}

object LowerVariables extends EntityTransformerPass(declFirst = false) {
  val name = "lower-variables"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || defn.asInstanceOf[DefnEntity].variant != EntityVariant.Fsm

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new LowerVariables
}
