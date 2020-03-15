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
// Lower assertions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.SequenceNumbers

import scala.collection.mutable

final class LowerAssert(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // List of assertion (enable signal, condition signals, message)
  private val assertions = mutable.ListBuffer[(Symbol, Symbol, Option[String])]()

  private val seqNum = new SequenceNumbers

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {

    case StmtAssert(Assert(cond, msg)) =>
      val n = seqNum.next
      val enSymbol = cc.newSymbol(s"_assert_en_$n", tree.loc)
      enSymbol.kind = TypeUInt(1)
      enSymbol.attr.combSignal set true
      val condSymbol = cc.newSymbol(s"_assert_cond_$n", cond.loc)
      condSymbol.kind = cond.tpe.underlying
      condSymbol.attr.combSignal set true
      assertions.append((enSymbol, condSymbol, msg))
      Thicket(
        List(
          StmtAssign(ExprSym(enSymbol), ExprInt(false, 1, 1)) regularize tree.loc,
          StmtAssign(ExprSym(condSymbol), cond) regularize cond.loc
        ))

    case defn: DefnEntity if assertions.nonEmpty =>
      val newBody = List from {
        defn.body.iterator concat {
          assertions.iterator flatMap {
            case (enSymbol, condSymbol, _) =>
              val enDefn = EntDefn(enSymbol.mkDefn) regularize enSymbol.loc
              val condDefn = EntDefn(condSymbol.mkDefn) regularize condSymbol.loc
              Iterator(enDefn, condDefn)
          }
        } concat {
          Iterator single {
            val asserts = List from {
              assertions.iterator flatMap {
                case (enSymbol, condSymbol, msgOpt) =>
                  Iterator(
                    StmtComment(s"Procedural assertion on line ${enSymbol.loc.line}"),
                    StmtIf(
                      ExprSym(enSymbol),
                      StmtAssert(Assert(ExprSym(condSymbol), msgOpt)) :: Nil,
                      Nil
                    ) regularize enSymbol.loc
                  )
              }
            }
            val rstInactive = cc.settings.resetStyle match {
              case ResetStyle.AsyncHigh | ResetStyle.SyncHigh => !ExprSym(defn.rst.get)
              case ResetStyle.AsyncLow | ResetStyle.SyncLow   => ExprSym(defn.rst.get)
            }
            val stmts = List(
              StmtComment("Assertions") regularize tree.loc,
              defn.go match {
                case Some(goSymbol) =>
                  StmtIf(ExprSym(goSymbol) && rstInactive, asserts, Nil) regularize tree.loc
                case None =>
                  StmtIf(rstInactive, asserts, Nil) regularize tree.loc
              }
            )
            TypeAssigner(
              EntClockedProcess(
                ExprSym(defn.clk.get) regularize tree.loc,
                None,
                stmts
              ) withLoc tree.loc)
          }
        }
      }
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case decl: DeclEntity if assertions.nonEmpty =>
      val newDecls = List from {
        decl.decls.iterator concat {
          assertions.iterator flatMap {
            case (enSymbol, condSymbol, _) =>
              val enDecl = enSymbol.mkDecl regularize enSymbol.loc
              val condDecl = condSymbol.mkDecl regularize condSymbol.loc
              Iterator(enDecl, condDecl)
          }
        }
      }
      TypeAssigner(decl.copy(decls = newDecls) withLoc tree.loc)

    //
    case _ => tree
  }

}

object LowerAssert extends EntityTransformerPass(declFirst = false) {
  val name = "lower-assert"

  override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerAssert
}
