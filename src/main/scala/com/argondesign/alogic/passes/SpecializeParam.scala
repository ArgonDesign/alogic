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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.PartialMatch

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

final class SpecializeEntity(bindings: Map[String, Expr], instLoc: Option[Loc])(
    implicit cc: CompilerContext)
    extends TreeTransformer
    with PartialMatch {

  override val typed = false
  override val checkRefs = false

  // Map from original symbol to the new symbol
  private[this] val symbolMap = mutable.Map[Symbol, Symbol]()

  // Only for computing name of specialized entity
  private[this] val paramValues = ListBuffer[(String, BigInt)]()

  // For sanity checking only
  private[this] val outerParams = mutable.Set[TermSymbol]()

  private[this] object TypeSpecializeEntity extends TreeInTypeTransformer(this)

  private[this] var entityLevel = 0

  // For type checking parameter assignments
  private[this] val typer = new Typer

  // Bail on errors
  var hadError = false

  // TODO: If the root entity does not have any parameters then we just need
  // to type check the const declarations

  override def skip(tree: Tree): Boolean = hadError

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
    // Stop if we had any errors
    ////////////////////////////////////////////////////////////////////////////

    case _ if hadError => tree

    ////////////////////////////////////////////////////////////////////////////
    // Change parameter declarations in the first level to constant declarations
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol, init) if entityLevel == 1 && symbol.kind.isParam => {
      // Specialize the parameter type itself
      val TypeParam(kind) = symbol.kind.asParam
      val newKind = TypeParam(kind rewrite TypeSpecializeEntity)

      // Figure out actual parameter value
      val newInit = bindings.get(symbol.name) orElse init orElse {
        // Must have a value by now (either from bindings or the default), so fail
        val msg = if (instLoc.isDefined) {
          s"Parameter '${symbol.name}' must be provided by instantiation of entity '${entitySymbol.name}' as it has no default value"
        } else {
          s"Top level entity '${entitySymbol.name}' requires default parameter values"
        }
        cc.error(instLoc getOrElse tree.loc, msg)
        hadError = true
        Option(ExprNum(false, 0) withLoc tree.loc)
      }

      // Create the new symbol
      val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      newSymbol.attr update symbol.attr // TODO: is this still needed?
      symbolMap(symbol) = newSymbol

      // Remember symbols we replaced
      outerParams add symbol

      // Create new decl
      val newDecl = Decl(newSymbol, newInit) withLoc tree.loc

      // Type check the new declaration and hence the parameter assignment.
      val typed = newDecl rewrite typer

      // Now change the symbol into a constant
      newSymbol.kind = TypeConst(newKind.kind)

      // Latch error
      hadError |= typed.tpe.isError

      // Remember final parameter value
      if (!hadError) {
        // .value might still fail due to for example out of range width inference
        // TODO: should check out of range width inference in the typer
        newInit.get.value foreach { v =>
          paramValues.append((symbol.name, v))
        }
      }

      // Must simplify the actual init expression in order to remove references
      // to external symbols
      TypeAssigner(newDecl.copy(init = newInit map { _.simplify }) withLoc tree.loc)
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
      val newDecl = Decl(newSymbol, init) withLoc tree.loc
      // Type check it if it's a const declaration in the outermost entity
      if (entityLevel == 1 && symbol.kind.isConst) {
        newDecl rewrite typer
      }
      // Done
      newDecl
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
      symbolMap.get(symbol) map {
        ExprRef(_) withLoc tree.loc
      } getOrElse tree
    }

    case Sym(symbol: TermSymbol) => {
      symbolMap.get(symbol) map {
        Sym(_) withLoc tree.loc
      } getOrElse tree
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone nested entities/Create the specialized entity
    ////////////////////////////////////////////////////////////////////////////

    case entity: EntityNamed => {
      val newName = if (entityLevel > 1 || paramValues.isEmpty) {
        entitySymbol.name
      } else {
        val suffix = paramValues.toList map { case (p, v) => s"${p}_${v}" } mkString cc.sep
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
    if (!hadError) {
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
}

object SpecializeParam extends Pass with FollowedBy {
  val name = "specialize-param"

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // '(entity, bindings)' -> 'specialized entity' map
    val specializations = mutable.Map[(TypeSymbol, Map[String, Expr]), Option[EntityNamed]]()

    // Returns the specialized entity, or None if an error happened
    def specialize(entitySymbol: TypeSymbol,
                   bindings: Map[String, Expr],
                   catalog: Map[TypeSymbol, EntityNamed],
                   instLoc: Option[Loc])(implicit cc: CompilerContext): Option[EntityNamed] = {

      // Here is how the specialization is actually done
      def specialized: Option[EntityNamed] = {
        // Get the definition of the entity
        val entity = catalog(entitySymbol)

        // Specialize the parameters of this entity. This replaces all
        // param decls with const decls in this entity and type checks
        // all const decls.
        val transformer = new SpecializeEntity(bindings, instLoc)
        val special = (entity rewrite transformer).asInstanceOf[EntityNamed]
        // Stop if we had an error
        if (transformer.hadError) return None

        // Extend the catalog with the immediately nested entities
        val extendedCatalog = catalog ++ {
          special.entities map { entity =>
            entity.symbol -> entity
          }
        }

        // Specialize nested entities according to instantiations in this entity,
        // gather new instances if there were no errors
        val newInstanceOpts = for {
          inst @ Instance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), pNames, pExprs) <- special.instances
        } yield {
          val bindings = (pNames zip pExprs).toMap
          // TODO: check pExprs are compile time constants
          specialize(eSymbol, bindings, extendedCatalog, Some(inst.loc)) map { newEntity =>
            iSymbol.kind = TypeInstance(newEntity.symbol)
            inst.copy(
              module = Sym(newEntity.symbol) withLoc inst.loc,
              paramNames = Nil,
              paramExprs = Nil
            ) withLoc inst.loc
          }
        }

        // Stop if specializing a nested entity failed
        if (newInstanceOpts exists { _.isEmpty }) return None

        // Gather the specialized nested entities
        val newEntities = special.entities flatMap {
          case e => specializations collect { case (k, v) if k._1 == e.symbol => v } flatten
        }

        // Get the new instances
        val newInstances = newInstanceOpts.flatten

        // Replace nested entities and instantiations with the specialized ones
        Some(special.copy(instances = newInstances, entities = newEntities) withLoc special.loc)
      }

      // If we have already performed this specialization, return that otherwise
      // do it. TODO: resovle the chicken and egg specialized and bindings.simplify
      specializations.getOrElseUpdate((entitySymbol, bindings), specialized)
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
    topSymbols foreach { specialize(_, Map.empty, catalog, None) }

    // Gather and return those specialized entities which are specializations
    // of entities we started with (i.e.: all file scope entities)
    catalog.keys flatMap { eSymbol =>
      specializations collect { case (k, v) if k._1 == eSymbol => v } flatten
    } toList
  }
}
