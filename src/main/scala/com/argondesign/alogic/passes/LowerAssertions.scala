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
// Lower assertions into separate clocked processes, and convert originals
// to assumptions to aid folding.
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
import scala.collection.mutable.ListBuffer

final class LowerAssertions(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // List of emitted assertion (enable signal, condition signals, message, comment)
  private val assertions = mutable.ListBuffer[(Symbol, Symbol, Option[String], String)]()

  private val assertSeqNum = new SequenceNumbers

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {

    // If assertions are not enabled, just convert them to assumptions if the
    // condition is pure, otherwise drop them
    case StmtAssertion(AssertionAssert(cond, _)) if !cc.settings.assertions =>
      if (cond.isPure) {
        StmtAssertion(AssertionAssume(cond)) regularize tree.loc
      } else {
        Stump
      }

    // If assertions are enabled, convert them to deferred assertions, but
    // leave in the original as an assume to be used by folding. The assume
    // will be removed by the RemoveAssume pass.
    case StmtAssertion(AssertionAssert(cond, msgOpt)) =>
      val prefix = s"_assert_${assertSeqNum.next}"
      val comment = s"'assert' on line ${tree.loc.line}"
      val enSymbol = cc.newSymbol(s"${prefix}_en", tree.loc)
      enSymbol.kind = TypeUInt(1)
      enSymbol.attr.combSignal set true
      val condSymbol = cc.newSymbol(s"${prefix}_cond", cond.loc)
      condSymbol.kind = cond.tpe.underlying
      condSymbol.attr.combSignal set true
      assertions.append((enSymbol, condSymbol, msgOpt, comment))
      val lb = new ListBuffer[Stmt]
      lb.append(StmtAssign(ExprSym(enSymbol), ExprInt(false, 1, 1)) regularize tree.loc)
      lb.append(StmtAssign(ExprSym(condSymbol), cond) regularize cond.loc)
      if (cond.isPure) {
        lb.append(StmtAssertion(AssertionAssume(cond)) regularize cond.loc)
      }
      Thicket(lb.toList)

    case defn: DefnEntity if assertions.nonEmpty =>
      val newBody = List from {
        defn.body.iterator concat {
          assertions.iterator flatMap {
            case (enSymbol, condSymbol, _, _) =>
              val enDefn = EntDefn(enSymbol.mkDefn) regularize enSymbol.loc
              val condDefn = EntDefn(condSymbol.mkDefn) regularize condSymbol.loc
              Iterator(enDefn, condDefn)
          }
        } concat {
          Iterator single {
            val asserts = List from {
              assertions.iterator flatMap {
                case (enSymbol, condSymbol, msgOpt, comment) =>
                  Iterator(
                    StmtComment(comment),
                    StmtIf(
                      ExprSym(enSymbol),
                      StmtAssertion(AssertionAssert(ExprSym(condSymbol), msgOpt)) :: Nil,
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
            case (enSymbol, condSymbol, _, _) =>
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

object LowerAssertions extends EntityTransformerPass(declFirst = false) {
  val name = "lower-assertions"

  override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerAssertions
}
