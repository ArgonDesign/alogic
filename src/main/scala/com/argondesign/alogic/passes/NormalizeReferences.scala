////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Convert references to term symbols external to an entity but local to an
//  enclosing entity to local references and wire through signals as necessary.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
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
      implicit
      cc: CompilerContext
    ): Map[Symbol, Set[Symbol]] = {
    require(decl.symbol eq defn.symbol)
    // Set of term symbols defined within this entity
    val definedSymbols = Set from {
      decl.decls.iterator map { _.symbol } filterNot { _.kind.isType }
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
        case ExprSym(symbol) if symbol.kind.isSnoop    => true
        case ExprSym(symbol) if symbol.kind.isPipeIn   => true
        case ExprSym(symbol) if symbol.kind.isPipeOut  => true
        case ExprSym(symbol) if symbol.kind.isConst    => true
        case ExprSym(symbol) if symbol.kind.isPipeVar  => false
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
      maps.foldLeft(Map.empty[Symbol, Set[Symbol]])(_ ++ _)
    }
    // Set of symbols referenced within or below this entity
    val usedSymbols = nestedResults.valuesIterator.fold(directlyReferencedSymbols)(_ union _)
    // Set of outer symbols that must be propagated to this entity
    val requiredSymbols = (usedSymbols diff definedSymbols) intersect outerSymbols
    // Include this entity in the result map
    nestedResults + (decl.symbol -> requiredSymbols)
  }

  def apply(
      decl: DeclEntity,
      defn: DefnEntity
    )(
      implicit
      cc: CompilerContext
    ): Map[Symbol, Set[Symbol]] = analyze(decl, defn, Set.empty[Symbol])

}

final class NormalizeReferencesA(requiredSymbolsMap: Map[Symbol, Set[Symbol]])
    extends StatefulTreeTransformer {

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
            .map(newDefn => TypeAssigner(EntSplice(newDefn) withLoc newDefn.loc))
          val sortedNewOtherDefns = newOtherDedns.toSeq
            .sortBy(d => (d.loc.start, d.symbol.name))
            .iterator
            .map(newDefn => TypeAssigner(EntSplice(newDefn) withLoc newDefn.loc))
          TypeAssigner(defn.copy(body = List from {
            sortedNewConstDefns ++ defn.body.iterator ++ sortedNewOtherDefns
          }) withLoc defn.loc)
      }

    // Wire through ports to instances of entities with propagated ports
    case EntSplice(defn: DefnInstance) =>
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
                case _: TypeIn | _: TypePipeIn | _: TypeSnoop =>
                  Some(
                    EntAssign(
                      ExprSym(defn.symbol) sel requiredSymbol.name,
                      ExprSym(outerSymbol)
                    ) regularize defn.loc
                  )
                case _: TypeOut | _: TypePipeOut =>
                  Some(
                    EntAssign(
                      ExprSym(outerSymbol),
                      ExprSym(defn.symbol) sel requiredSymbol.name
                    ) regularize defn.loc
                  )
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

  override def finalCheck(tree: Tree): Unit = {
    assert(newDeclsStack.isEmpty)
    assert(newDefnsStack.isEmpty)
  }

}

final class NormalizeReferencesB(
    globalReplacements: TrieMap[Symbol, Symbol],
    propagatedSymbols: Set[Symbol])
    extends StatefulTreeTransformer {

  // We need to replace:
  // - All symbols which have storage stripped
  // - All entity symbols which contain a symbol with storage stripped, but
  //   only if the entity is defined in the input tree. We will do global
  //   replacement later
  // - All instance symbols of local entities replaces

  // Set of symbols to replace. Initialize to port symbols needing storage
  // stripped
  private val symbolsToReplace = mutable.Set from {
    propagatedSymbols.iterator filter {
      _.kind match {
        case TypeOut(_, _, st) => st != StorageTypeDefault
        case _                 => false
      }
    }
  }

  // Replace marked symbols, or instances thereof
  override def replace(symbol: Symbol): Boolean = symbolsToReplace(symbol) || {
    symbol.kind match {
      case TypeEntity(symbol, _) => symbolsToReplace(symbol)
      case _                     => false
    }
  }

  // Just a predicate
  private def hasReplacedMember(kind: Type): Boolean = kind match {
    case TypeType(TypeEntity(_, publicSymbols)) => publicSymbols exists symbolsToReplace
    case _                                      => false
  }

  // Mark root symbol as replaced if needed
  override protected def start(tree: Tree): Unit = tree match {
    case Decl(symbol) if hasReplacedMember(symbol.kind) => symbolsToReplace add symbol
    case _                                              =>
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case decl: DeclEntity =>
        // Marked nested definitions as replaced if needed
        decl.decls foreach {
          case Decl(symbol) if hasReplacedMember(symbol.kind) => symbolsToReplace add symbol
          case _                                              =>
        }
        // Memorize the entity symbol replacement for the global pass
        orig.get(decl.symbol) foreach { oldSymbol =>
          assert(!(globalReplacements contains oldSymbol), globalReplacements foreach println)
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

final class NormalizeReferencesC(globalReplacements: collection.Map[Symbol, Symbol])
    extends StatefulTreeTransformer {

  // Now replace instances with replaced entities
  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Update remaining instance types
    //////////////////////////////////////////////////////////////////////////

    case decl @ DeclInstance(_, ExprSym(eSymbol)) =>
      globalReplacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    //
    case _ => tree
  }

}

object NormalizeReferences {

  def apply(): Pass[Pairs, Pairs] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()
    val requiredSymbolMaps = TrieMap[Symbol, Map[Symbol, Set[Symbol]]]()

    new EntityTransformerPass(declFirst = true) {
      val name = "normalize-references-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = {
        // Get decl/defn
        val decl = symbol.decl.asInstanceOf[DeclEntity]
        val defn = symbol.defn.asInstanceOf[DefnEntity]
        // Analyze and figure out what symbols need to be propagated
        val requiredSymbols = Analyze(decl, defn) ensuring { _(symbol).isEmpty }
        requiredSymbolMaps(symbol) = requiredSymbols
        //
        new NormalizeReferencesA(requiredSymbols)
      }
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "normalize-references-b"

      private lazy val propagatedSymbols = Set from {
        requiredSymbolMaps.valuesIterator flatMap { _.valuesIterator flatMap { _.iterator } }
      }

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new NormalizeReferencesB(globalReplacements, propagatedSymbols)
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "normalize-references-c"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new NormalizeReferencesC(globalReplacements)
    }
  }

}
