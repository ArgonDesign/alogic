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
// Specialize parameters and process 'gen' constructs
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types.TypeInstance
import com.argondesign.alogic.transform.Generate
import com.argondesign.alogic.transform.SpecializeParam
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.language.postfixOps

object Specialize extends Pass {
  val name = "specialize"

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // '(entity, bindings)' -> 'specialized entity' map
    val specializations = mutable.LinkedHashMap[(TypeSymbol, Map[String, Expr]), Option[Entity]]()

    // Returns the specialized entity, or None if an error happened
    def specialize(entitySymbol: TypeSymbol,
                   bindings: Map[String, Expr],
                   catalog: Map[TypeSymbol, Entity],
                   instLoc: Option[Loc])(implicit cc: CompilerContext): Option[Entity] = {

      // Here is how the specialization is actually done
      def specialized: Option[Entity] = {
        // Get the definition of the entity
        val entity = catalog(entitySymbol)

        val specialized = {
          // Specialize the parameters of this entity (but not nested entities).
          // This transform replaces all param decls with const decls in this
          // entity and type checks all const decls in this entity.
          val specializeParamsTransform = new SpecializeParam(bindings, instLoc)
          val tmp = (entity rewrite specializeParamsTransform).asInstanceOf[Entity]
          // Stop if we had an error
          if (specializeParamsTransform.hadError) return None

          // Process Gen nodes in the specialized entity (but not inside nested
          // entities which might have further parameters)
          Generate(tmp) match {
            case None                 => return None // Stop if errors
            case Some(entity: Entity) => entity
            case _                    => unreachable
          }
        }

        // Extend the catalog with the immediately nested entities
        val extendedCatalog = catalog ++ {
          specialized.entities map { entity =>
            entity.symbol -> entity
          }
        }

        // Specialize nested entities according to instantiations in this entity,
        // gather new instances if there were no errors
        val newInstanceOpts = for {
          inst @ EntInstance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), pNames, pExprs) <- specialized.instances
        } yield {
          val bindings = (pNames zip pExprs).toMap
          // TODO: check pExprs are compile time constants
          specialize(eSymbol, bindings, extendedCatalog, Some(inst.loc)) map { newEntity =>
            iSymbol.kind = TypeInstance(newEntity.symbol)
            inst.copy(
              entity = Sym(newEntity.symbol) withLoc inst.loc,
              paramNames = Nil,
              paramExprs = Nil
            ) withLoc inst.loc
          }
        }

        // Stop if specializing a nested entity failed
        if (newInstanceOpts exists { _.isEmpty }) return None

        // Gather the specialized nested entities
        val newEntities = specialized.entities flatMap { entity =>
          specializations collect {
            case (k, specialOpt) if k._1 == entity.symbol =>
              specialOpt map { special =>
                EntEntity(special) withLoc special.loc
              }
          } flatten
        }

        // Replace nested entities and instantiations with the specialized ones
        val newBody = specialized.body filter {
          case _: EntEntity   => false // Drop original entities
          case _: EntInstance => false // Drop original instances
          case _              => true
        } concat newEntities concat newInstanceOpts.flatten

        Some(specialized.copy(body = newBody) withLoc specialized.loc)
      }

      // If we have already performed this specialization, return that otherwise
      // do it. TODO: resolve the chicken and egg specialized and bindings.simplify
      specializations.getOrElseUpdate((entitySymbol, bindings), specialized)
    }

    // Build catalog if file scope entities
    val catalog = Map from {
      trees collect {
        case Root(_, entity @ Entity(Sym(symbol: TypeSymbol), _)) => symbol -> entity
      }
    }

    // Gather the top level entity symbols
    val topSymbols = trees collect {
      case Root(_, Entity(Sym(symbol: TypeSymbol), _)) => symbol
    }

    // Recursively specialize from all top level entities,
    // this populates the 'specializations' map
    topSymbols foreach { specialize(_, Map.empty, catalog, None) }

    // Gather and return those specialized entities which are specializations
    // of entities we started with (i.e.: all file scope entities)
    List from {
      catalog.keys flatMap { s =>
        specializations collect { case (k, v) if k._1 == s => v } flatten
      }
    }
  }
}
