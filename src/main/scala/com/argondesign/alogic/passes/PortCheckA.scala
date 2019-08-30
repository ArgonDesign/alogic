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
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut

import scala.collection.mutable

final class PortCheckA(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] def multipleSinkError(lhs: Expr, loc: Loc): Unit = {
    cc.error(lhs, s"'${lhs.toSource}' has multiple sinks", s"Previous '->' is at: ${loc.prefix}")
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: Root   => false
    case _: Entity => false
    case _         => true
  }

  override def enter(tree: Tree): Unit = tree match {

    case entity: Entity => {

      //////////////////////////////////////////////////////////////////////////
      // Check multiple sinks - sync ready ports only
      //////////////////////////////////////////////////////////////////////////

      // Map of symbols appearing on the left of a connect that is declared by
      // us to the location of the connect
      val drivOurs = mutable.Map[TermSymbol, Loc]()
      // Map of instance/port symbol pairs on the left of a connect to the
      // location of the connect
      val drivInst = mutable.Map[(TermSymbol, TermSymbol), Loc]()

      for {
        connect @ EntConnect(lhs, _) <- entity.connects
      } {
        lhs match {
          case rhs @ ExprSym(symbol: TermSymbol) => {
            if (symbol.kind.isIn && symbol.kind.asInstanceOf[TypeIn].fct == FlowControlTypeReady) {
              drivOurs.get(symbol) foreach { multipleSinkError(lhs, _) }
              drivOurs(symbol) = connect.loc
            }
          }
          case InstancePortRef(iSymbol, pSymbol) => {
            if (pSymbol.kind.asInstanceOf[TypeOut].fct == FlowControlTypeReady) {
              val key = (iSymbol, pSymbol)
              drivInst.get(key) foreach { multipleSinkError(lhs, _) }
              drivInst(key) = connect.loc
            }
          }
          case _ => ()
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Check storage specifiers
      //////////////////////////////////////////////////////////////////////////

      for {
        decl @ Decl(symbol: TermSymbol, _) <- entity.declarations
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

object PortCheckA extends TreeTransformerPass {
  val name = "port-check-a"
  def create(implicit cc: CompilerContext) = new PortCheckA
}
