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
// Gather all parameter bindings from instances, and the default parameter
// bindings from entities. Specialize top level entities.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.ReplaceTermRefs

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

final class SpecializeParamA(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity   => false
    case _: Instance => false
    case _           => true
  }

  override def enter(tree: Tree): Unit = tree match {
    case Instance(Sym(iSymbol), Sym(eSymbol), paramNames, paramExprs) => {
      val entityKind = eSymbol.kind.asInstanceOf[TypeEntity]

      if (entityKind.paramSymbols.nonEmpty) {
        // Gather particular parameter bindings
        val paramMap = (paramNames zip paramExprs).toMap

        val paramBindings = {
          val pairs = for {
            symbol <- entityKind.paramSymbols
            expr <- paramMap.get(symbol.name)
          } yield {
            symbol -> expr.simplify
          }
          pairs.toMap
        }

        // Append attribute to entity so we can specialize it
        eSymbol synchronized {
          eSymbol.attr.paramBindings append paramBindings
        }

        // Append attribute to instance so we can replace the entity with the specialized version
        iSymbol.attr.paramBinding set paramBindings
      }
    }

    case entity: Entity => {
      // Collect the default parameter bindings
      val defaultBindings = {
        val pairs = entity.declarations collect {
          case Decl(symbol, Some(init)) if symbol.kind.isInstanceOf[TypeParam] => {
            symbol -> init
          }
        }
        ListMap(pairs: _*)
      }

      if (defaultBindings.nonEmpty) {
        // Assign owner of each parameter symbol
        defaultBindings.keysIterator foreach {
          _.attr.owner set entity
        }

        // Flatten the default bindings
        @tailrec
        def flattenBindings(bindings: ListMap[TermSymbol, Expr]): ListMap[TermSymbol, Expr] = {
          // Simplify the expressions
          val simplified = bindings map { case (symbol, expr) => symbol -> expr.simplify }

          // Collect any referenced parameters
          val referenced = simplified.valuesIterator flatMap { expr =>
            expr collect {
              case ExprRef(symbol) if symbol.kind.isInstanceOf[TypeParam] => symbol
            }
          }

          if (referenced.isEmpty) {
            // If no parameters are referenced, we are done, but check
            // we know all parameters values by now
            for (expr <- simplified.valuesIterator if expr.value.isEmpty) {
              cc.error(expr, "Parameter initializer is not a compile time constant")
            }
            simplified
          } else {
            // Otherwise expand the bindings them using themselves
            val replace = new ReplaceTermRefs(bindings)
            val flattened = bindings map {
              case (symbol, expr) => symbol -> expr.rewrite(replace).asInstanceOf[Expr]
            }
            flattenBindings(flattened)
          }
        }
        val flattenedBindings = flattenBindings(defaultBindings)

        // Assign the defaultParamBindings attribute
        entitySymbol.attr.defaultParamBindings set flattenedBindings

        // Specialize top level entities with default parameters
        if (entitySymbol.attr.topLevel.isSet) {
          cc.warning(entity.ref,
                     s"Parameters of top level module '${entitySymbol.name}' will " +
                       "be specialized using default initializers")

          entitySymbol synchronized {
            entitySymbol.attr.paramBindings append Map.empty[TermSymbol, Expr]
          }
        }
      }
    }

    case _ =>
  }

}

object SpecializeParamA extends TreeTransformerPass {
  val name = "specialize-param-a"
  def create(implicit cc: CompilerContext) = new SpecializeParamA
}
