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
// Specialize entities with all parameter bindings, replace params with consts.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.InlineParam
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class SpecializeParam(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity if entitySymbol.hasAttr("param-bindings") => {
      // TODO: This now assumes that Parameters are not specified as expressions
      // of other parameters, but are always constants
      val bindings = entitySymbol.attr[List[Map[Symbol, Option[Expr]]]]("param-bindings")

      val newEntities = for (binding <- bindings) yield {
        val actual = binding filter { _._2.isDefined } mapValues { _.get }
        val newEntity = entity rewrite (new InlineParam(actual)) match {
          case entity: Entity => entity
          case _              => unreachable
        }

        val Sym(newSymbol) = newEntity.ref
        newSymbol.delAttr("param-bindings")

        newEntity
      }

      TypeAssigner(Thicket(newEntities) withLoc tree.loc)
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ Decl(_, _: TypeParam, _) => {
        cc.ice(node, "param remains")
      }
    }
  }

}
