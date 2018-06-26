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
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.CloneEntity
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.language.postfixOps

final class SpecializeParam(
    val defaultBindingsMap: Map[TypeSymbol, Bindings],
    val specializationMap: Map[TypeSymbol, Map[Bindings, EntityNamed]]
)(implicit cc: CompilerContext)
    extends TreeTransformer {

  override val typed = false
  override val checkRefs = false

  override def skip(tree: Tree): Boolean = tree match {
    case _: Root     => false
    case _: Entity   => false
    case _: Instance => false
    case _           => true
  }

  override def transform(tree: Tree): Tree = tree match {
    case Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol: TypeSymbol), paramNames, paramExprs) => {
      val eKind = eSymbol.kind.asInstanceOf[TypeEntity]
      if (eKind.paramSymbols.isEmpty) {
        // Instantiated entity has no parameters
        tree
      } else {
        // Find the specialized entity, if any
        val sSymbol = specializationMap.get(eSymbol) map { bindingsMap =>
          // Gather (now specialized) particular parameter bindings
          val paramBindings = Bindings {
            val paramMap = (paramNames zip paramExprs).toMap
            for {
              symbol <- eKind.paramSymbols
              expr <- paramMap.get(symbol.name)
            } yield {
              symbol -> expr.simplify
            }
          }
          // Complete them with the default bindings
          val completeBindings = defaultBindingsMap(eSymbol) ++ paramBindings
          bindingsMap(completeBindings).symbol
        } getOrElse {
          eSymbol
        }

        if (sSymbol == eSymbol) {
          tree
        } else {
          // Update type of instance
          iSymbol.kind = TypeInstance(sSymbol)
          // Rewrite tree
          Instance(Sym(iSymbol), Sym(sSymbol), Nil, Nil) regularize tree.loc
        }
      }
    }

    case _ => tree
  }
}

object SpecializeParam extends Pass {
  val name = "specialize-param"

  // Recursively specialize entities directly nested within this entity
  def specialize(entity: EntityNamed)(implicit cc: CompilerContext): EntityNamed = {
    assert {
      entity.declarations forall {
        case Decl(s, _) => !s.kind.isParam
        case _          => unreachable
      }
    }

    // Gather the entity symbols we are specializing on this round
    val eSymbols = entity.entities map { _.symbol }

    // Since we need the values of const and param symbols for parameter
    // specialization, we type check their declarations here. This also has
    // the side-effect of assigning their init attributes, meaning we can
    // compute values dependent on const symbols.
    entity.entities.par flatMap { entity =>
      entity.declarations collect {
        case decl @ Decl(symbol, _) if symbol.kind.isParam => Typer(decl)
        case decl @ Decl(symbol, _) if symbol.kind.isConst => Typer(decl)
      }
    }

    // Similarly, type check parameter assignments in instantiations
    def checkParamAssigns(entity: EntityNamed): Unit = {
      for {
        Instance(_, Sym(eSymbol: TypeSymbol), pNames, pExprs) <- entity.instances
        if eSymbols contains eSymbol
        pSymbols = eSymbol.kind.asInstanceOf[TypeEntity].paramSymbols
        (pName, pExpr) <- pNames zip pExprs
      } {
        // Get the parameter symbol
        val pSymbolOpt = pSymbols.collectFirst { case symbol if symbol.name == pName => symbol }
        if (pSymbolOpt.isEmpty) {
          cc.error(pExpr, s"No parameter named '${pName}' in entity '${eSymbol.name}'")
        }

        Typer(pExpr)
        // TODO: Add ArgAssign Tree node and type check
        //      pSymbolOpt foreach { pSymbol =>
        //        val ref = ExprRef(pSymbol) withLoc pExpr.loc
        //        val ass = StmtAssign(ref, pExpr) withLoc pExpr.loc
        //        Typer(ass)
        //      }
      }
      entity.entities foreach checkParamAssigns
    }
    checkParamAssigns(entity)

    cc.stopIfError()

    // Gather the 'entity symbol' -> 'default bindings' map
    val defaultBindingsMap: Map[TypeSymbol, Bindings] = {
      val pairs = entity.entities.par map { entity =>
        // Build the default bindings for this entity
        val bindings = Bindings {
          entity.declarations collect {
            case Decl(symbol, Some(init)) if symbol.kind.isParam => symbol -> init
          }
        }

        // Assign owner of each parameter symbol
        bindings.keysIterator foreach { _.attr.owner set entity }

        // Make pair with expanded the bindings
        entity.symbol -> bindings.expand
      } filter {
        // Drop non-parametrized entities
        _._2.nonEmpty
      }

      // Check we now know all initializer values
      for {
        (_, bindings) <- pairs
        expr <- bindings.valuesIterator if expr.value.isEmpty
      } {
        cc.error(expr, "Parameter initializer is not a compile time constant")
      }

      pairs.seq.toMap
    }

    // Gather the 'instance symbol' -> 'particular bindings'
    val instanceBindingsMap: Map[TermSymbol, Bindings] = {
      // Build a map from instance symbols to the particular parameter bindings
      // used for any instance created under and including the given entity.
      // Ensure these are complete bindings by using default parameter values
      // when needed
      def gatherInstantiationBindings(entity: EntityNamed): Map[TermSymbol, Bindings] = {
        // Build the map of instances in this entity
        val thisMap = entity.instances collect {
          case Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol: TypeSymbol), pNames, pExprs)
              if defaultBindingsMap contains eSymbol => {
            iSymbol -> Bindings {
              val pMap = (pNames zip pExprs).toMap
              eSymbol.kind.asInstanceOf[TypeEntity].paramSymbols map { pSymbol =>
                val expr = pMap get pSymbol.name map {
                  _.simplify
                } getOrElse {
                  defaultBindingsMap(eSymbol)(pSymbol)
                }
                pSymbol -> expr
              }
            }
          }
        } toMap
        // Apply recursively to all nested entities
        val childMaps = entity.entities map gatherInstantiationBindings
        // Merge all maps
        (thisMap /: childMaps)(_ ++ _)
      }
      gatherInstantiationBindings(entity)
    }

    // Compound the above 'instance symbol' -> 'particular bindings' map to
    // an 'entity symbol' -> 'Set(particular bindings)' map
    val instanceBindingsSet = instanceBindingsMap.toSet
    val entityBindingsMap: Map[TypeSymbol, Set[Bindings]] = instanceBindingsSet groupBy {
      _._1.kind.asInstanceOf[TypeInstance].entitySymbol
    } mapValues { _ map { _._2 } }

    // Given a set of parameter bindings that might reference parameters in
    // other bindings, return a set of bindings which have been expanded using
    // all possible specializations of the referenced parameters.
    @tailrec
    def expandBindings(bindingsSet: Set[Bindings]): Set[Bindings] = {
      // Simplify all expressions in all bindings
      val simplified = bindingsSet map { _ mapValues { _.simplify } }

      // Iterator of all parameters referenced in the bindings. We only use the
      // first ever element of this, if it exits.
      val referenced = simplified.iterator flatMap { _.valuesIterator } flatMap { expr =>
        expr collectFirst { case ExprRef(symbol) if symbol.kind.isParam => symbol }
      }

      if (!referenced.hasNext) {
        // If no parameters are referenced, we are done,
        // but check we now know all parameter values
        for {
          bindings <- simplified
          expr <- bindings.valuesIterator if expr.value.isEmpty
        } {
          cc.error(expr, "Parameter value is not a compile time constant")
        }
        simplified
      } else {
        // Expand based on the owner of the first parameter we encounter
        val oEntitySymbol = referenced.next().attr.owner.value.symbol
        // Parametrized top level entities have no instances (are not in
        // entityBindingsMap), so we use the set of empty bindings for them
        val oBindingsSet = entityBindingsMap.getOrElse(oEntitySymbol, Set(Bindings.empty))
        val oDefaultBindings = defaultBindingsMap(oEntitySymbol)

        // For each specialization of the entity defining the referenced
        // parameter, specialize the current bindings
        val expanded = oBindingsSet flatMap { oBindings =>
          val completedOBindings = Bindings(oDefaultBindings ++ oBindings)
          simplified map { bindings =>
            bindings map {
              case (symbol, expr) => symbol -> (expr given completedOBindings)
            }
          }
        }

        // Keep going expanding the rest of the references
        expandBindings(expanded)
      }
    }

    // Specialize all entities by cloning them, substituting all relevant
    // instance bindings for parameters. Build a map from
    // 'entity symbol' -> 'bindings' -> 'specialized entity'
    val specializationMap: Map[TypeSymbol, Map[Bindings, EntityNamed]] = {
      val pairs = entity.entities.par map { entity =>
        val eSymbol = entity.symbol
        val eKind = eSymbol.kind.asInstanceOf[TypeEntity]

        // If an entity without instances (i.e.: a top-level) have
        // parameters, specialize them with default parameters
        lazy val fallBack = if (eKind.paramSymbols.nonEmpty) {
          cc.warning(eSymbol.loc,
                     s"Parameters of top level module '${eSymbol.name}' will " +
                       "be specialized using default initializers")
          Set(defaultBindingsMap(eSymbol))
        } else {
          Set.empty[Bindings]
        }

        // Get the bindings set used by all instantiations of this entity
        val bindingsSet = entityBindingsMap.getOrElse(eSymbol, fallBack)

        if (bindingsSet.isEmpty) {
          // Leave non-parametrized entities alone, but add them to the
          // result map with empty bindings
          eSymbol -> Map(Bindings.empty -> entity)
        } else {
          // Create the specialized entities and build the map from
          // bindings -> specialized entity symbol
          val specializations = {
            // Expand the bindings with bindings of referenced
            // parameters of other entities (if any)
            val expandedBindingsSet = expandBindings(bindingsSet)

            // Create the specialized entities and build the map
            expandedBindingsSet map { bindings =>
              val suffixes = for (pSymbol <- eKind.paramSymbols) yield {
                s"${pSymbol.name}_${bindings(pSymbol).value.get}"
              }
              val cloneName = (eSymbol.name :: suffixes) mkString cc.sep
              val cloner = new CloneEntity(typed = false, bindings, cloneName)
              bindings -> entity.rewrite(cloner).asInstanceOf[EntityNamed]
            } toMap
          }

          eSymbol -> specializations
        }
      }
      pairs.seq.toMap
    }

    // Gather all resulting entities
    val specialized = specializationMap.values flatMap { _.values } toList

    // Rewrite instantiations in specialized entities and then recursively
    // specialize their nested entities
    val specialized2 = {
      val par = specialized.par map { entity =>
        val tt = new SpecializeParam(defaultBindingsMap, specializationMap)
        (entity rewrite tt).asInstanceOf[EntityNamed]
      } map {
        specialize
      }
      par.seq.toList
    }

    // Update and rewrite instantiations in the original entity
    val result = TypeAssigner {
      entity.copy(entities = specialized2) withLoc entity.loc
    } rewrite new SpecializeParam(defaultBindingsMap, specializationMap)

    // Done
    result.asInstanceOf[EntityNamed]
  }

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // We are going to iteratively specialized entities at increasing nesting
    // levels, starting at the entities in file scope, followed by entities
    // nested one deep, and so on. To keep iteration uniform, we build an
    // artificial root entity which looks as if all other entities were
    // nested inside it, but is otherwise empty.

    val entities = trees map {
      case Root(_, entity: EntityNamed) => entity
      case _                            => unreachable
    }

    val loc = Loc.synthetic

    val rootSymbol = {
      cc.newTypeSymbol("@@@root@@@", loc, TypeEntity("@@@root@@@", Nil, Nil))
    }
    rootSymbol.attr.variant set "network"

    val root = TypeAssigner {
      EntityNamed(rootSymbol, Nil, Nil, Nil, Nil, Nil, Nil, entities, Map()) withLoc loc
    }

    specialize(root).entities
  }

}
