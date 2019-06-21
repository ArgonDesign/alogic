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
// As the name suggests, specialize all entities according to the
// actual parameter values used at their instantiations
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.PartialMatch
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.language.postfixOps

final class SpecializeEntity(bindings: Map[String, Expr])(implicit cc: CompilerContext)
    extends TreeTransformer
    with PartialMatch {

  override val typed = false
  override val checkRefs = false

  // Map from original symbol to the new symbol
  private[this] val symbolMap = mutable.Map[Symbol, Symbol]()

  // Only for computing name of specialized entity
  private[this] val paramValues = Stack[(String, BigInt)]()

  // For sanity checking only
  private[this] val outerParams = mutable.Set[TermSymbol]()

  private[this] object TypeSpecializeEntity extends TreeInTypeTransformer(this)

  private[this] var entityLevel = 0

  // Do nothing if the root entity has no parameters
  override def skip(tree: Tree): Boolean = tree match {
    case entity: EntityNamed if entityLevel == 0 => {
      !(entity.declarations exists {
        case decl: Decl => decl.symbol.kind.isParam
        case _          => unreachable
      })
    }
    case _ => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case entity: EntityNamed => {
      entityLevel += 1
      // Clone new functions up front, as they can be referenced before definition
      for (Function(Sym(symbol: TermSymbol), _) <- entity.functions) {
        symbolMap(symbol) = cc.newSymbolLike(symbol)
      }
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Change parameter declarations in the first level to constant declarations
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol, init) if entityLevel == 1 && symbol.kind.isParam => {
      // Change to const declaration
      val TypeParam(kind) = symbol.kind
      val newKind = TypeConst(kind rewrite TypeSpecializeEntity)

      // Figure out actual value
      val newInit = bindings.get(symbol.name) orElse init
      // TODO: check here newInit is not None when params without initializers are implemented

      // Create the new symbol
      val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      newSymbol.attr update symbol.attr // TODO: is this still needed?
      symbolMap(symbol) = newSymbol

      // Remember final parameter value
      paramValues.push((symbol.name, newInit.get.value.get))

      // Remember symbols we replaced
      outerParams add symbol

      // Create new decl
      Decl(newSymbol, newInit) withLoc tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone other declaration symbols
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol, init) => {
      // Rewrite references in types
      val newKind = symbol.kind rewrite TypeSpecializeEntity
      // Clone the symbol
      val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      newSymbol.attr update symbol.attr
      symbolMap(symbol) = newSymbol
      Decl(newSymbol, init) withLoc tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone instance symbols
    ////////////////////////////////////////////////////////////////////////////

    case inst @ Instance(Sym(symbol: TermSymbol), _, _, _) => {
      // Clone the symbol
      val newSymbol = cc.newSymbolLike(symbol)
      symbolMap(symbol) = newSymbol
      // Rewrite with new symbol
      inst.copy(ref = Sym(newSymbol) withLoc tree.loc) withLoc tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Update references
    ////////////////////////////////////////////////////////////////////////////

    case ExprRef(symbol: TermSymbol) => {
      symbolMap.get(symbol) map { ExprRef(_) withLoc tree.loc } getOrElse tree
    }

    case Sym(symbol: TermSymbol) => {
      symbolMap.get(symbol) map { Sym(_) withLoc tree.loc } getOrElse tree
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone nested entities/Create the specialized entity
    ////////////////////////////////////////////////////////////////////////////

    case entity: EntityNamed => {
      val newName = if (entityLevel > 1) {
        entitySymbol.name
      } else {
        val suffix = paramValues.toList.reverse map { case (p, v) => s"${p}_${v}" } mkString cc.sep
        entitySymbol.name + cc.sep + suffix
      }

      // Update instances of nested entities to instantiate the cloned entity
      val instances = for {
        inst @ Instance(Sym(iSymbol), Sym(eSymbol), _, _) <- entity.instances
      } yield {
        symbolMap get eSymbol map { nSymbol =>
          iSymbol.kind = TypeInstance(nSymbol.asInstanceOf[TypeSymbol])
          inst.copy(module = Sym(nSymbol) withLoc inst.loc) withLoc inst.loc
        } getOrElse inst
      }

      val newKind = {
        val portSymbols = for {
          Decl(symbol, _) <- entity.declarations
          if symbol.kind.isInstanceOf[TypeIn] || symbol.kind.isInstanceOf[TypeOut]
        } yield symbol

        val paramSymbols = for {
          Decl(symbol, _) <- entity.declarations
          if symbol.kind.isInstanceOf[TypeParam]
        } yield symbol

        TypeEntity(newName, portSymbols, paramSymbols)
      }

      val newSymbol = cc.newTypeSymbol(newName, tree.loc, newKind)
      newSymbol.attr update entitySymbol.attr
      symbolMap(entitySymbol) = newSymbol

      entity.copy(
        symbol = newSymbol,
        instances = instances
      ) withLoc tree.loc
    } followedBy {
      entityLevel -= 1
    }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(entityLevel == 0)

    tree visitAll {
      case node @ ExprRef(symbol: TermSymbol) if outerParams contains symbol => {
        cc.ice(node, "Reference to parameter remains")
      }
      case node @ Sym(symbol: TermSymbol) if outerParams contains symbol => {
        cc.ice(node, "Sym to parameter remains")
      }
      case node @ Decl(symbol, _) if outerParams contains symbol => {
        cc.ice(node, "Outer parameter declaration remains")
      }
    }
  }
}

final class ConstTyper(implicit cc: CompilerContext) extends TreeTransformer {
  // This is a special wrapper around the Typer that only type checks
  // const declaration in the outermost entity.

  override val typed = false
  override val checkRefs = false

  val typer = new Typer

  var inEntity = false

  override def skip(tree: Tree): Boolean = tree match {
    case _: EntityNamed  => inEntity
    case Decl(symbol, _) => !symbol.kind.isConst
    case _               => true
  }

  override def enter(tree: Tree): Unit = tree match {
    case _: Entity => inEntity = true
    case _         => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    case _: EntityNamed => tree
    case _              => tree rewrite typer
  }
}

object SpecializeParam extends Pass with FollowedBy {
  val name = "specialize-param"

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // '(entity, bindings)' -> 'specialized entity' map
    val specializations = mutable.Map[(TypeSymbol, Map[String, Expr]), EntityNamed]()

    // Returns the specialized entity, or None if an error happened
    def specialize(entitySymbol: TypeSymbol,
                   bindings: Map[String, Expr],
                   catalog: Map[TypeSymbol, EntityNamed])(
        implicit cc: CompilerContext): Option[EntityNamed] = {
      // Type check the bindings
      val checkedBindings = {
        val typer = new Typer
        bindings.mapValues { _.rewrite(typer).asInstanceOf[Expr] }
      }

      // TODO: check parameter assignments

      // Stop if there are any type errors
      if (checkedBindings.values exists { _.tpe.isError }) return None

      // Simplify the bindings
      val simplifiedBindings = checkedBindings.mapValues { _.simplify }

      // Ensure we know all parameter values
      assert(simplifiedBindings.values forall { _.value.isDefined })

      // The key for the 'specializations' map
      val tag = (entitySymbol, simplifiedBindings)

      // Here is how the specialization is done ...
      lazy val specialized = {
        // Get the definition of the entity
        val entity = catalog(entitySymbol)

        // Specialize the parameters of this entity
        // This replaces all param decls with const decls in this entity
        val specialized0 = {
          val transformer = new SpecializeEntity(simplifiedBindings)
          (entity rewrite transformer).asInstanceOf[EntityNamed]
        }

        // Type check the const declarations in the specialized entity,
        // so we can use these constants to compute actual parameters to
        // instances in this entity
        val specialized1 = {
          val transformer = new ConstTyper
          (specialized0 rewrite transformer).asInstanceOf[EntityNamed]
        }

        // Stop if there are any type errors
        if (specialized1.existsAll { case tree: Tree if tree.hasTpe => tree.tpe.isError }) {
          None
        } else {
          // Collect any immediately nested entities
          val childCatalog = {
            specialized1.entities map { entity: EntityNamed =>
              entity.symbol -> entity
            }
          } toMap

          // Extend the catalog with the immediately nested entities
          val extendedCatalog = catalog ++ childCatalog

          // Specialize nested entities according to instantiations in this entity,
          // gather new instances
          val newInstances = for {
            inst @ Instance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), pNames, pExprs) <- specialized1.instances
          } yield {
            val bindings = (pNames zip pExprs).toMap
            specialize(eSymbol, bindings, extendedCatalog) match {
              case Some(newEntity) => {
                iSymbol.kind = TypeInstance(newEntity.symbol)
                inst.copy(
                  module = Sym(newEntity.symbol) withLoc inst.loc,
                  paramNames = Nil,
                  paramExprs = Nil
                ) withLoc inst.loc
              }
              case None => inst
            }
          }

          // Gather the specialized nested entities
          val newEntities = specialized1.entities flatMap {
            case e => specializations collect { case (k, v) if k._1 == e.symbol => v }
          }

          // Replace nested entities and instantiations with the specialized ones
          val specialized2 = specialized1.copy(
            instances = newInstances,
            entities = newEntities
          ) withLoc specialized1.loc

          // Add this specialization to the cache
          specializations(tag) = specialized2

          // Return the specialized entity
          Some(specialized2)
        }
      }

      // If we have already performed this specializations, return that otherwise do it
      specializations.get(tag) orElse specialized
    }

    // Build catalog if file scope entities
    val catalog = trees collect {
      case Root(_, entity: EntityNamed) => entity.symbol -> entity
    } toMap

    // Gather the top level entity symbols
    val topSymbols = trees collect {
      case Root(_, entity: EntityNamed) if entity.symbol.attr.topLevel.isSet => entity.symbol
    }

    // Recursively specialize from all top level entities,
    // this populates the 'specializations' map
    topSymbols foreach { specialize(_, Map.empty, catalog) }

    // Gather and return those specialized entities which are specializations
    // of entities we started with (i.e.: all file scope entities)
    catalog.keys flatMap { eSymbol =>
      specializations collect { case (k, v) if k._1 == eSymbol => v }
    } toList
  }
}
