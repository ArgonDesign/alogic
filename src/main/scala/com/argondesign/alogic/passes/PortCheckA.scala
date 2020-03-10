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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class PortCheckA(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private[this] def multipleSinkError(lhs: Expr, loc: Loc): Unit = {
    cc.error(lhs,
             s"Port with 'sync ready' flow control has multiple sinks",
             s"Previous '->' is at: ${loc.prefix}")
  }

  // TODO: add back
//  override def skip(tree: Tree): Boolean = tree match {
//    case _: Root   => false
//    case _: Decl => false
//    case _: Decl => false
//    case _         => true
//  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {

      case _: DefnEntity | _: DefnSingleton =>
        // Gather the parts

        val connects = tree match {
          case d: DefnEntity    => d.connects
          case d: DefnSingleton => d.connects
          case _                => unreachable
        }

        val decls = tree match {
          case d: DefnEntity    => d.symbol.decl.decls
          case d: DefnSingleton => d.symbol.decl.decls
          case _                => unreachable
        }

        //////////////////////////////////////////////////////////////////////////
        // Check multiple sinks - sync ready ports only
        //////////////////////////////////////////////////////////////////////////

        // Map of symbols appearing on the left of a connect that is declared by
        // us to the location of the connect
        val drivOurs = mutable.Map[Symbol, Loc]()
        // Map of instance/port symbol pairs on the left of a connect to the
        // location of the connect
        val drivInst = mutable.Map[(Symbol, Symbol), Loc]()

        for {
          connect @ EntConnect(lhs, _) <- connects
        } {
          lhs match {
            case ExprSym(symbol) =>
              if (symbol.kind.isIn && symbol.kind.asInstanceOf[TypeIn].fc == FlowControlTypeReady) {
                drivOurs.get(symbol) foreach { multipleSinkError(lhs, _) }
                drivOurs(symbol) = connect.loc
              }
            case InstancePortRef(iSymbol, pSymbol) =>
              if (pSymbol.kind.asInstanceOf[TypeOut].fc == FlowControlTypeReady) {
                val key = (iSymbol, pSymbol)
                drivInst.get(key) foreach { multipleSinkError(lhs, _) }
                drivInst(key) = connect.loc
              }
            case _ => ()
          }
        }

        //////////////////////////////////////////////////////////////////////////
        // Check storage specifiers
        //////////////////////////////////////////////////////////////////////////

        for {
          decl <- decls
        } {
          decl.symbol.kind match {
            case TypeOut(_, FlowControlTypeReady, StorageTypeWire) =>
              cc.error(decl, "'sync ready' port cannot use 'wire' storage specifier")
            case TypeOut(_, FlowControlTypeAccept, st) if st != StorageTypeWire =>
              cc.error(decl, "'sync accept' port must use 'wire' storage specifier")
            case _ => ()
          }
        }

      case _ =>
    }
    None
  }

}

object PortCheckA extends PairTransformerPass {
  val name = "port-check-a"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new PortCheckA
    (transformer(decl), transformer(defn))
  }
}
