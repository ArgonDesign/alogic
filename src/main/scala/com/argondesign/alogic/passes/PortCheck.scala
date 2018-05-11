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
// Further port usage checks
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class PortCheck(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] def multipleDriverError(rhs: Expr, loc: Loc): Unit = {
    cc.error(rhs, s"'${rhs.toSource}' has multiple drivers", s"Previous '->' is at: ${loc.prefix}")
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity => false
    case _         => true
  }

  override def enter(tree: Tree): Unit = tree match {

    case entity: Entity => {
      // Map of symbols appearing on the right of a connect that is declared by
      // us to the location of the connect
      val sinkOurs = mutable.Map[TermSymbol, Loc]()
      // Map of instance/port symbol pairs on the right of a connect to the
      // location of the connect
      val sinkInst = mutable.Map[(TermSymbol, TermSymbol), Loc]()

      for {
        Connect(_, rhss) <- entity.connects
        rhs <- rhss
      } {
        rhs match {
          case rhs @ ExprRef(symbol: TermSymbol) => {
            sinkOurs.get(symbol) foreach { multipleDriverError(rhs, _) }
            sinkOurs(symbol) = tree.loc
          }
          case InstancePortRef(iSymbol, pSymbol) => {
            val key = (iSymbol, pSymbol)
            sinkInst.get(key) foreach { multipleDriverError(rhs, _) }
            sinkInst(key) = tree.loc
          }
          case _ => unreachable
        }
      }

      for {
        decl @ Decl(symbol: TermSymbol, _) <- entity.declarations
        if !(sinkOurs contains symbol)
      } {
        symbol.kind match {
          case TypeOut(_, FlowControlTypeReady, StorageTypeWire) => {
            cc.error(decl, "'sync ready' port cannot use 'wire' storage specifier")
          }
          case TypeOut(_, FlowControlTypeAccept, st) if st != StorageTypeWire => {
            cc.error(decl, "'sync accept' port must use 'wire' storage specifier")
          }
          case _ => ()
        }
      }
    }

    case _ => ()
  }

}

object PortCheck extends TreeTransformerPass {
  val name = "port-check"
  def create(implicit cc: CompilerContext) = new PortCheck
}
