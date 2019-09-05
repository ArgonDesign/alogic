////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Replace external port references to dict ports with a reference to the
// expanded port
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.transform.DictIdxValues

final class ResolveDictPorts(implicit cc: CompilerContext) extends TreeTransformer {

  override val typed: Boolean = false

  override def skip(tree: Tree): Boolean = tree match {
    case _: Root       => false
    case _: Entity     => false
    case _: EntEntity  => false
    case _: EntConnect => false
    case _: Expr       => false
    case _             => true
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprSelect(expr, sel, Nil) =>
      expr match {
        // Error for referencing x.p#[n] as x.p__n
        case ExprSym(iSymbol) if iSymbol.kind.isInstance =>
          iSymbol.kind.asInstance.entitySymbol.kind.asEntity.portSymbols exists { pSymbol =>
            !pSymbol.attr.sourceName.isSet && pSymbol.name == sel
          } pipe {
            case true => tree
            case false =>
              cc.error(tree, s"No port named '$sel' on instance '${expr.toSource}'")
              ExprError() withLoc tree.loc
          }
        case _ => tree
      }

    case ExprSelect(expr, sel, idxs) =>
      val res = expr match {
        case ExprSym(iSymbol) if iSymbol.kind.isInstance =>
          DictIdxValues(idxs) map { idxValues =>
            val eSymbol = iSymbol.kind.asInstance.entitySymbol
            eSymbol.kind.asEntity.portSymbols collectFirst {
              case portSymbol if portSymbol.attr.sourceName.contains((sel, idxValues)) =>
                ExprSelect(expr, portSymbol.name, Nil)
            } getOrElse {
              val srcName = idxValues.mkString(sel + "#[", ", ", "]")
              cc.error(tree, s"No port named '$srcName' on instance '${expr.toSource}'")
              ExprError()
            }
          } getOrElse {
            ExprError()
          }
        case _ =>
          cc.error(tree, "Illegal use of '.' lookup with dictionary indices")
          ExprError()
      }
      res withLoc tree.loc

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visitAll {
      case node @ ExprSelect(_, _, idxs) if idxs.nonEmpty =>
        cc.ice(node, s"ExprSelect with indices remains")
    }
  }
}

object ResolveDictPorts extends TreeTransformerPass {
  val name = "resolve-dict-ports"
  def create(implicit cc: CompilerContext) = new ResolveDictPorts
}
