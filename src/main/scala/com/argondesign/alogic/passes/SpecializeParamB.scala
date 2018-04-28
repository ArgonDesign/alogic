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
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.CloneEntity
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable.ListMap

final class SpecializeParamB(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity if entitySymbol.attr.paramBindings.isSet => {
      // TODO: This now assumes that Parameters are not specified as expressions
      // of other parameters, but are always constants

      // Pairs of param symbols and the corresponding default values
      lazy val pSymbolsAndDefaults = for {
        Decl(symbol, initOpt) <- entity.declarations
        if symbol.denot.kind.isInstanceOf[TypeParam]
        init <- initOpt
      } yield {
        symbol -> init
      }

      // TODO: empty bindings if required ...

      // Create the specialized entities and build the map from
      // bindings -> specialized entity symbol
      val specializations = {
        val pairs = for (partialBindings <- entitySymbol.attr.paramBindings.value) yield {
          // Compute the complete bindings by adding the default values if
          // needed. Take care that these are in param symbol definition order
          // as CloneEntity computes the entity name suffix based on the order
          // of the binding.
          val completeBindings = {
            val pairs = for ((pSymbol, default) <- pSymbolsAndDefaults) yield {
              pSymbol -> partialBindings.getOrElse(pSymbol, default)
            }
            ListMap(pairs: _*)
          }

          val newEntity = entity rewrite new CloneEntity(completeBindings) match {
            case entity: Entity => entity
            case _              => unreachable
          }

          val Sym(newSymbol) = newEntity.ref

          newSymbol.attr.paramBindings.clear()

          partialBindings -> newEntity
        }
        pairs.toMap
      }

      entitySymbol.attr.paramBinding.clear()
      entitySymbol.attr.specMap set specializations

      TypeAssigner(Thicket(specializations.values.toList) withLoc tree.loc)
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeParam] => {
        cc.ice(node, "param remains")
      }
    }
  }

}
