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

object Specialize extends Pass {
  val name = "specialize"

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // '(entity, bindings)' -> 'specialized tree' map
    val specializations =
      mutable.LinkedHashMap[(TypeSymbol, Map[String, Expr]), Option[Either[Root, Entity]]]()

    // Returns the specialized tree, or None if an error happened
    def specialize(
        entitySymbol: TypeSymbol,
        bindings: Map[String, Expr],
        catalog: Map[TypeSymbol, Either[Root, Entity]],
        instLoc: Option[Loc])(implicit cc: CompilerContext): Option[Either[Root, Entity]] = {

      // Here is how the specialization is actually done
      lazy val specialized: Option[Either[Root, Entity]] = {
        {
          // Get the definition of the entity
          val tree = catalog(entitySymbol)

          // Specialize the parameters of this entity (but not nested entities).
          // This transform replaces all param decls with const decls in this
          // entity and type checks all const decls (and leading defns) in this
          // entity.
          val specializeParamsTransform = new SpecializeParam(bindings, instLoc)
          val tmp = tree fold ({ root: Root =>
            Left((root rewrite specializeParamsTransform).asInstanceOf[Root])
          }, { entity: Entity =>
            Right((entity rewrite specializeParamsTransform).asInstanceOf[Entity])
          })

          if (specializeParamsTransform.hadError) {
            // Stop if we had an error
            None
          } else {
            // Process Gen nodes in the specialized entity (but not inside nested
            // entities which might have further parameters)
            tmp fold (Generate(_), Generate(_)) map {
              case root: Root     => Left(root)
              case entity: Entity => Right(entity)
              case _              => unreachable
            }
          }
        } flatMap { specialized: Either[Root, Entity] =>
          // Extract the specialized entity
          val specializedEntity = specialized fold (_.entity, identity)

          // Extend the catalog with the immediately nested entities
          val extendedCatalog = catalog ++ {
            specializedEntity.entities map { entity =>
              entity.symbol -> Right(entity)
            }
          }

          // Specialize nested entities according to instantiations in this entity,
          // gather new instances if there were no errors
          val newInstances = specializedEntity.instances flatMap {
            case inst @ EntInstance(Sym(iSymbol, Nil),
                                    Sym(eSymbol: TypeSymbol, Nil),
                                    pNames,
                                    pExprs) =>
              val bindings = (pNames zip pExprs).toMap
              // TODO: check pExprs are compile time constants
              specialize(eSymbol, bindings, extendedCatalog, Some(inst.loc)) map { newTree =>
                val newEntitySymbol = newTree.fold(_.entity.symbol, _.symbol)
                iSymbol.kind = TypeInstance(newEntitySymbol)
                inst.copy(
                  entity = Sym(newEntitySymbol, Nil) withLoc inst.loc,
                  paramNames = Nil,
                  paramExprs = Nil
                ) withLoc inst.loc
              }
            case _ => unreachable
          }

          if (specializedEntity.instances.lengthIs != newInstances.length) {
            // Stop if specializing a nested entity failed
            None
          } else {
            // Gather the specialized nested entities
            val newEntities = specializedEntity.entities.iterator flatMap { entity =>
              specializations.iterator flatMap {
                case (k, specialOpt) if k._1 == entity.symbol =>
                  specialOpt map {
                    case Right(special) => EntEntity(special) withLoc special.loc
                    case Left(_)        => unreachable
                  }
                case _ => None
              }
            }

            // Replace nested entities and instantiations with the specialized ones
            val newBody = specializedEntity.body filter {
              case _: EntEntity   => false // Drop original entities
              case _: EntInstance => false // Drop original instances
              case _              => true
            } concat newEntities concat newInstances

            // Create new entity
            val newEntity = specializedEntity.copy(body = newBody) withLoc specializedEntity.loc

            specialized fold (
              root => Some(Left(root.copy(entity = newEntity) withLoc root.loc)),
              _ => Some(Right(newEntity))
            )
          }
        }
      }

      // If we have already performed this specialization, return that otherwise
      // do it. TODO: resolve the chicken and egg specialized and bindings.simplify
      specializations.getOrElseUpdate((entitySymbol, bindings), specialized)
    }

    // Build catalog of file scope entities
    val catalog = Map from {
      trees collect {
        case root @ Root(_, Entity(Sym(symbol: TypeSymbol, _), _)) => symbol -> Left(root)
      }
    }

    // Gather the top level entity symbols
    val topSymbols = trees collect {
      case Root(_, Entity(Sym(symbol: TypeSymbol, _), _)) if symbol.attr.topLevel.isSet => symbol
    }

    // Recursively specialize from all top level entities,
    // this populates the 'specializations' map
    topSymbols foreach { specialize(_, Map.empty, catalog, None) }

    // Return those specialized entities which are specializations of file
    // scope entities (i.e.: those which are Root nodes)
    List from {
      specializations.valuesIterator collect { case Some(Left(root)) => root }
    }
  }
}
