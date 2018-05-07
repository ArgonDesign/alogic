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
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.CloneEntity
import com.argondesign.alogic.transform.ReplaceTermRefs
import com.argondesign.alogic.typer.TypeAssigner

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

final class SpecializeParamB(implicit cc: CompilerContext) extends TreeTransformer {

  private type Bindings = ListMap[TermSymbol, Expr]

  // Compute the complete bindings by adding the default values if
  // needed. Take care that these are in param symbol definition order
  // as CloneEntity computes the entity name suffix based on the order
  // of the binding.
  def completeBindings(partialBindings: Map[TermSymbol, Expr],
                       defaultBindins: Bindings): Bindings = {
    defaultBindins map {
      case (symbol, expr) => symbol -> partialBindings.getOrElse(symbol, expr)
    }
  }

  // Given a set of bindings that might reference outer parameters, return a
  // set of bindings which have been expanded using all possible specializations
  // of the referenced parameters.
  @tailrec
  def expandBindings(bindingsSet: Set[Bindings]): Set[Bindings] = {
    // Simplify all expressions
    val simplified = bindingsSet map { _ map { case (symbol, expr) => symbol -> expr.simplify } }

    // Iterator of all parameters referenced in the bindings
    val referenced = simplified.iterator flatMap { _.valuesIterator } flatMap { expr =>
      expr collectFirst {
        case ExprRef(Sym(symbol)) if symbol.kind.isInstanceOf[TypeParam] => symbol
      }
    }

    if (!referenced.hasNext) {
      // If no parameters are referenced, we are done
      simplified
    } else {
      // Expand based on the owner of the first parameter we encounter
      val Sym(oEntitySymbol) = referenced.next().attr.owner.value.ref
      val oBindingsSet = oEntitySymbol.attr.paramBindings.value.toSet
      val oDefaultBindings = oEntitySymbol.attr.defaultParamBindings.value

      // For each specialization of the entity defining the referenced
      // parameter, specialize the current bindings
      val expanded = oBindingsSet flatMap { oBindings =>
        val completedOBindings = completeBindings(oBindings, oDefaultBindings)
        val replace = new ReplaceTermRefs(completedOBindings)
        simplified map { bindings =>
          bindings map { case (symbol, expr) => symbol -> expr.rewrite(replace).asInstanceOf[Expr] }
        }
      }

      // Keep going expanding the rest of the references
      expandBindings(expanded)
    }
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity => false
    case _         => true
  }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity if entitySymbol.attr.paramBindings.isSet => {
      // Create the specialized entities and build the map from
      // bindings -> specialized entity symbol
      val specializations = {
        // Get the fully expanded parameter binding sets
        val paramBindingsSet = {
          // Get all parameter bindings of this entity
          val bindingsSet = entitySymbol.attr.paramBindings.value.toSet
          // Get the default parameter bindings of this entity
          val defaultBindings = entitySymbol.attr.defaultParamBindings.value
          // Ensure all bindings are complete
          val completedBindingsSet = bindingsSet map { completeBindings(_, defaultBindings) }
          // Expand the bindings with bindings of the outer parameters (if any)
          expandBindings(completedBindingsSet)
        }

        // Create the specialized entities and build the map
        val pairs = for (bindings <- paramBindingsSet) yield {
          // Create specialized entity
          val newEntity = entity.rewrite(new CloneEntity(bindings)).asInstanceOf[Entity]

          // Nuke the inherited paramBindings and defaultBindings attributes
          val Sym(newSymbol) = newEntity.ref
          newSymbol.attr.paramBindings.clear()
          newSymbol.attr.defaultParamBindings.clear()

          // Create the map
          bindings -> newEntity
        }
        pairs.toMap
      }

      entitySymbol.attr.specMap set specializations

      TypeAssigner(Thicket(specializations.values.toList) withLoc tree.loc)
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ Decl(symbol, _) if symbol.kind.isInstanceOf[TypeParam] => {
        cc.ice(node, "param remains")
      }
    }
  }

}

object SpecializeParamB extends TreeTransformerPass {
  val name = "specialize-param-b"
  def create(implicit cc: CompilerContext) = new SpecializeParamB
}
