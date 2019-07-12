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
import com.argondesign.alogic.util.PartialMatch
import com.argondesign.alogic.util.unreachable

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
    case entity: Entity => {
      // Clone root entity up-font to simplify re-writing
      if (entityLevel == 0) {
        symbolMap(entity.symbol) = cc.newSymbolLike(entity.symbol)
      }

      entityLevel += 1

      // Clone new functions, instances and nested entities up front,
      // as they can be referenced before definition
      entity.body foreach {
        case EntFunction(Sym(symbol: TermSymbol), _) =>
          symbolMap(symbol) = cc.newSymbolLike(symbol)
        case EntInstance(Sym(symbol: TermSymbol), _, _, __) =>
          symbolMap(symbol) = cc.newSymbolLike(symbol)
        case EntEntity(Entity(Sym(symbol: TypeSymbol), _)) =>
          symbolMap(symbol) = cc.newSymbolLike(symbol)
        case _ =>
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
    // Update references
    ////////////////////////////////////////////////////////////////////////////

    case ExprRef(symbol) => {
      symbolMap.get(symbol) map {
        ExprRef(_) withLoc tree.loc
      } getOrElse tree
    }

    case Sym(symbol) => {
      symbolMap.get(symbol) map {
        Sym(_) withLoc tree.loc
      } getOrElse tree
    }

    ////////////////////////////////////////////////////////////////////////////
    // Update instances
    ////////////////////////////////////////////////////////////////////////////

    case EntInstance(Sym(iSymbol), Sym(eSymbol), _, _) => {
      // The 2 Sym instances have already been rewritten, we only need to
      // update the type of the iSymbol to refer to cloned entities
      symbolMap get eSymbol foreach {
        case nSymbol: TypeSymbol => iSymbol.kind = TypeInstance(nSymbol)
        case _                   => unreachable
      }

      // No need to re-write
      tree
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone nested entities/Create the specialized entity
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // Update name of nested entities
      if (entityLevel == 1 && paramValues.nonEmpty) {
        entity.symbol rename {
          entity.symbol.name + cc.sep + {
            paramValues.toList map { case (p, v) => s"${p}_${v}" } mkString cc.sep
          }
        }
      }

      // Update type of entity to refer to the cloned symbols
      entity.symbol.kind = {
        val portSymbols = for {
          Decl(symbol, _) <- entity.declarations
          if symbol.kind.isIn || symbol.kind.isOut
        } yield symbol

        val paramSymbols = for {
          Decl(symbol, _) <- entity.declarations
          if symbol.kind.isParam
        } yield symbol

        TypeEntity(entity.symbol.name, portSymbols, paramSymbols)
      }

      entity
    } tap { _ =>
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

object SpecializeParam extends Pass {
  val name = "specialize-param"

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // '(entity, bindings)' -> 'specialized entity' map
    val specializations = mutable.Map[(TypeSymbol, Map[String, Expr]), Option[Entity]]()

    // Returns the specialized entity, or None if an error happened
    def specialize(entitySymbol: TypeSymbol,
                   bindings: Map[String, Expr],
                   catalog: Map[TypeSymbol, Entity],
                   instLoc: Option[Loc])(implicit cc: CompilerContext): Option[Entity] = {

      // Here is how the specialization is actually done
      def specialized: Option[Entity] = {
        // Get the definition of the entity
        val entity = catalog(entitySymbol)

        // Specialize the parameters of this entity. This replaces all
        // param decls with const decls in this entity and type checks
        // all const decls.
        val transformer = new SpecializeEntity(bindings, instLoc)
        val special = (entity rewrite transformer).asInstanceOf[Entity]
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
          inst @ EntInstance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), pNames, pExprs) <- special.instances
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
        val newEntities = special.entities flatMap { entity =>
          specializations collect {
            case (k, specializedOpt) if k._1 == entity.symbol =>
              specializedOpt map { specialized =>
                EntEntity(specialized) withLoc specialized.loc
              }
          } flatten
        }

        val newBody = special.body filter {
          case _: EntEntity   => false
          case _: EntInstance => false
          case _              => true
        } concat newEntities concat newInstanceOpts.flatten

        // Replace nested entities and instantiations with the specialized ones
        Some(special.copy(body = newBody) withLoc special.loc)
      }

      // If we have already performed this specialization, return that otherwise
      // do it. TODO: resolve the chicken and egg specialized and bindings.simplify
      specializations.getOrElseUpdate((entitySymbol, bindings), specialized)
    }

    // Build catalog if file scope entities
    val catalog = trees collect {
      case Root(_, entity @ Entity(Sym(symbol: TypeSymbol), _)) => symbol -> entity
    } toMap

    // Gather the top level entity symbols
    val topSymbols = trees collect {
      case Root(_, Entity(Sym(symbol: TypeSymbol), _)) if symbol.attr.topLevel.isSet => symbol
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
