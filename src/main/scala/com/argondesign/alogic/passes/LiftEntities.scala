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
// Lift nested entities, wire through directly accessed ports
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

private object Analyze {
  // Given a decl and defn for an entity, and the set of symbols defined in
  // entities enclosing this entity, return a map from entity symbol to
  // sets of symbols that need to be propagated to that entity. The returned
  // map contains all entities defined nested inside the given decl/defn, as
  // well as the entity defined by the given decl/defn.
  private def analyze(
      decl: DeclEntity,
      defn: DefnEntity,
      outerSymbols: Set[Symbol]
  )(
      implicit cc: CompilerContext
  ): Map[Symbol, Set[Symbol]] = {
    require(decl.symbol eq defn.symbol)
    // Set of term symbols defined within this entity
    val definedSymbols = Set from {
      decl.decls.iterator filter {
        case _: DeclEntity => false
        case _: DeclRecord => false
        case _             => true
      } map { _.symbol }
    }
    // Set of symbols directly referenced within this entity
    val directlyReferencedSymbols = Set from {
      {
        decl flatCollect {
          case e: ExprSym                 => Some(e)
          case d: DeclEntity if d ne decl => None // Stop descent
        }
      } concat {
        defn flatCollect {
          case e: ExprSym                 => Some(e)
          case d: DefnEntity if d ne defn => None // Stop descent
        }
      } filter {
        case ExprSym(symbol) if symbol.kind.isIn       => true
        case ExprSym(symbol) if symbol.kind.isOut      => true
        case ExprSym(symbol) if symbol.kind.isConst    => true
        case ExprSym(symbol) if symbol.kind.isXenoFunc => true
        case e @ ExprSym(symbol) =>
          if (outerSymbols(symbol)) {
            symbol.kind match {
              case _: TypeEntity => cc.error(e, "Cannot access outer instance directly.")
              case _             => cc.error(e, "Cannot access outer name directly.")
            }
          }
          false
      } map {
        _.symbol
      }
    }
    // The results for nested entities
    val nestedResults = {
      val newOuterSymbols = outerSymbols union definedSymbols
      val maps = decl.entities map { decl =>
        analyze(decl, decl.symbol.defn.asInstanceOf[DefnEntity], newOuterSymbols)
      }
      maps.foldLeft(Map.empty[Symbol, Set[Symbol]]) { _ ++ _ }
    }
    // Set of symbols referenced within or below this entity
    val usedSymbols = nestedResults.valuesIterator.fold(directlyReferencedSymbols) { _ union _ }
    // Set of outer symbols that must be propagated to this entity
    val requiredSymbols = (usedSymbols diff definedSymbols) intersect outerSymbols
    // Include this entity in the result map
    nestedResults + (decl.symbol -> requiredSymbols)
  }

  def apply(
      decl: DeclEntity,
      defn: DefnEntity
  )(
      implicit cc: CompilerContext
  ): Map[Symbol, Set[Symbol]] = analyze(decl, defn, Set.empty[Symbol])
}

class LiftEntitiesA(
    requiredSymbolsMap: Map[Symbol, Set[Symbol]]
)(
    implicit cc: CompilerContext
) extends StatefulTreeTransformer {

  // Map of ('containing entity', 'referenced symbol') -> 'propagated symbol'
  private val propMap: Map[(Symbol, Symbol), Symbol] = requiredSymbolsMap flatMap {
    case (entitySymbol, requiredSymbols) =>
      requiredSymbols map { symbol =>
        (entitySymbol, symbol) -> symbol.dup
      }
  }

  // Set of entity symbols to replace
  private val entitiesToReplace = (requiredSymbolsMap filter { _._2.nonEmpty }).keySet

  // Replace all entity symbols which have propagated members
  // Also replace all instances of the same
  override def replace(symbol: Symbol): Boolean = entitiesToReplace(symbol) || {
    symbol.kind match {
      case TypeEntity(eSymbol, _) => entitiesToReplace(eSymbol)
      case _                      => false
    }
  }

  private val newDeclsStack = mutable.Stack[Set[Decl]]()
  private val newDefnsStack = mutable.Stack[Set[Defn]]()

  // Note: We build decls/defns up front in enter as the defn of constants
  // is required in order to work out widths (and hence types) of some
  // expressions, so defns of constants must be available before their use.
  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      // Build declarations of additional symbols added to this entity up front
      case decl: DeclEntity =>
        orig get decl.symbol match {
          case None => newDeclsStack push Set.empty
          case Some(oldSymbol) =>
            newDeclsStack push {
              requiredSymbolsMap(oldSymbol) map { symbol =>
                withEnclosingSymbol(decl.symbol) {
                  walk(TypeAssigner {
                    symbol.decl.cpy(symbol = propMap((oldSymbol, symbol))) withLoc symbol.decl.loc
                  }).asInstanceOf[Decl]
                }
              }
            }
        }

        // Also, rename to contain parent name
        if (enclosingSymbols.nonEmpty) {
          decl.symbol.name = entitySymbol.name + cc.sep + decl.symbol.name
        }

      // Build definitions of additional symbols added to this entity up front
      case defn: DefnEntity =>
        orig get defn.symbol match {
          case None => newDefnsStack push Set.empty
          case Some(oldSymbol) =>
            newDefnsStack push {
              requiredSymbolsMap(oldSymbol) map { symbol =>
                withEnclosingSymbol(defn.symbol) {
                  walk(TypeAssigner {
                    symbol.defn.cpy(symbol = propMap((oldSymbol, symbol))) withLoc symbol.decl.loc
                  }).asInstanceOf[Defn]
                }
              }
            }
        }

      //
      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Add declarations of required symbols into the receiving entity
    case decl: DeclEntity =>
      newDeclsStack.pop() match {
        case empty if empty.isEmpty => decl
        case newDecls =>
          val (newConstDecls, newOtherDecls) = newDecls partition {
            case _: DeclConst => true
            case _            => false
          }
          val sortedNewConstDecls =
            newConstDecls.toSeq.sortBy(d => (d.loc.start, d.symbol.name)).iterator
          val sortedNewOtherDecls =
            newOtherDecls.toSeq.sortBy(d => (d.loc.start, d.symbol.name)).iterator
          TypeAssigner(decl.copy(decls = List from {
            sortedNewConstDecls ++ decl.decls.iterator ++ sortedNewOtherDecls
          }) withLoc decl.loc)
      }

    // Add definitions of required symbols into the receiving entity
    case defn: DefnEntity =>
      newDefnsStack.pop() match {
        case empty if empty.isEmpty => defn
        case newDefns =>
          val (newConstDefns, newOtherDedns) = newDefns partition {
            case _: DefnConst => true
            case _            => false
          }
          val sortedNewConstDefns = newConstDefns.toSeq
            .sortBy(d => (d.loc.start, d.symbol.name))
            .iterator
            .map(newDefn => TypeAssigner(EntDefn(newDefn) withLoc newDefn.loc))
          val sortedNewOtherDefns = newOtherDedns.toSeq
            .sortBy(d => (d.loc.start, d.symbol.name))
            .iterator
            .map(newDefn => TypeAssigner(EntDefn(newDefn) withLoc newDefn.loc))
          TypeAssigner(defn.copy(body = List from {
            sortedNewConstDefns ++ defn.body.iterator ++ sortedNewOtherDefns
          }) withLoc defn.loc)
      }

    // Wire through ports to instances of entities with propagated ports
    case EntDefn(defn: DefnInstance) =>
      orig get defn.symbol match {
        case None => tree
        case Some(oldSymbol) =>
          val eSymbol =
            orig.getOrElse(oldSymbol.kind.asEntity.symbol, oldSymbol.kind.asEntity.symbol)
          val reSymbol = orig.getOrElse(entitySymbol, entitySymbol)
          val newConns = List from {
            requiredSymbolsMap(eSymbol).iterator flatMap { requiredSymbol =>
              val outerSymbol = propMap.getOrElse((reSymbol, requiredSymbol), requiredSymbol)
              outerSymbol.kind match {
                case _: TypeIn =>
                  Some(
                    EntConnect(
                      ExprSym(outerSymbol),
                      List(ExprSym(defn.symbol) select requiredSymbol.name)) regularize defn.loc)
                case _: TypeOut =>
                  Some(
                    EntConnect(ExprSym(defn.symbol) select requiredSymbol.name,
                               List(ExprSym(outerSymbol))) regularize defn.loc)
                case _: TypeConst    => None
                case _: TypeXenoFunc => None
                case _               => unreachable
              }
            }
          }
          Thicket(tree :: newConns)
      }

    // Rewrite references to propagated symbols
    case ExprSym(symbol) =>
      val reSymbol = orig.getOrElse(entitySymbol, entitySymbol)
      propMap.get((reSymbol, symbol)) match {
        case None            => tree
        case Some(newSymbol) => TypeAssigner(ExprSym(newSymbol) withLoc tree.loc)
      }

    //
    case _ => tree
  }

  override protected def finish(tree: Tree): Tree = tree match {
    case decl: DeclEntity =>
      def flatten(decl: DeclEntity): List[DeclEntity] = {
        val (nested, rest) = decl.decls partitionMap {
          case entity: DeclEntity => Left(entity)
          case other              => Right(other)
        }
        TypeAssigner(decl.copy(decls = rest) withLoc decl.loc) :: (nested flatMap flatten)
      }
      Thicket(flatten(decl))
    case defn: DefnEntity =>
      def flatten(defn: DefnEntity): List[DefnEntity] = {
        val (nested, rest) = defn.body partitionMap {
          case EntDefn(entity: DefnEntity) => Left(entity)
          case other                       => Right(other)
        }
        TypeAssigner(defn.copy(body = rest) withLoc defn.loc) :: (nested flatMap flatten)
      }
      Thicket(flatten(defn))
    case _ => unreachable
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(newDeclsStack.isEmpty)
    assert(newDefnsStack.isEmpty)
  }
}

class LiftEntitiesB(
    globalReplacements: mutable.Map[Symbol, Symbol],
    propagatedSymbols: Set[Symbol]
)(
    implicit cc: CompilerContext
) extends StatefulTreeTransformer {

  // Output ports with storage that have been pushed into nested entities need
  // to loose their storage and turn into wire ports, we collect these in a set
  private val stripStorageSymbols = propagatedSymbols filter {
    _.kind match {
      case TypeOut(_, _, st) => st != StorageTypeDefault
      case _                 => false
    }
  }

  // Replace:
  // - All symbols which have storage stripped
  // - All symbols which contain a symbol with storage stripped
  override def replace(symbol: Symbol): Boolean = {
    stripStorageSymbols(symbol) || {
      symbol.kind match {
        case TypeType(TypeEntity(_, publicSymbols)) =>
          enclosingSymbols.isEmpty && (publicSymbols exists stripStorageSymbols)
        case _ => false
      }
    }
  }

  // Note: We build decls/defns up front in enter as the defn of constants
  // is required in order to work out widths (and hence types) of some
  // expressions, so defns of constants must be available before their use.
  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case decl: DeclEntity =>
        orig.get(decl.symbol) foreach { oldSymbol =>
          assert(!(globalReplacements contains oldSymbol), oldSymbol)
          globalReplacements(oldSymbol) = decl.symbol
        }
      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Strip storage from output ports pushed into nested entities
    case decl @ DeclOut(symbol, _, _, _) =>
      orig.get(symbol) match {
        case None => tree
        case _    => TypeAssigner(decl.copy(st = StorageTypeDefault) withLoc tree.loc)
      }

    //
    case _ => tree
  }
}

final class LiftEntitiesC(
    globalReplacements: collection.Map[Symbol, Symbol],
)(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Update instance types
    //////////////////////////////////////////////////////////////////////////

    case decl @ DeclInstance(_, ExprSym(eSymbol)) =>
      globalReplacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    //
    case _ => tree
  }
}

object LiftEntities {

  def apply(): Pass[List[(Decl, Defn)], List[(Decl, Defn)]] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()
    val requiredSymbolMaps = TrieMap[Symbol, Map[Symbol, Set[Symbol]]]()

    new EntityTransformerPass(declFirst = true) {
      val name = "lift-entities-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = {
        // Get decl/defn
        val decl = symbol.decl.asInstanceOf[DeclEntity]
        val defn = symbol.defn.asInstanceOf[DefnEntity]
        // Analyze and figure out what symbols need to be propagated
        val requiredSymbols = Analyze(decl, defn) ensuring { _(symbol).isEmpty }
        requiredSymbolMaps(symbol) = requiredSymbols
        //
        new LiftEntitiesA(requiredSymbols)
      }
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "lift-entities-b"

      lazy val propagatedSymbols = Set from {
        requiredSymbolMaps.valuesIterator flatMap { _.valuesIterator flatMap { _.iterator } }
      }

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LiftEntitiesB(globalReplacements, propagatedSymbols)
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "lift-entities-c"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LiftEntitiesC(globalReplacements)
    }
  }
}
