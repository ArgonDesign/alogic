////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2016 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Transform parameters of top level entity to constants using given bindings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

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

final class SpecializeParam(
    bindings: Map[String, Expr],
    instLoc: Option[Loc]
)(implicit cc: CompilerContext)
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

  private[this] var entityLevel = 0

  // While this is true, we need to type check definitions as param/const
  // declarations might depend on the defined type
  private[this] var checkDefn = true

  // For type checking parameter assignments
  private[this] val typer = new Typer

  // Bail on errors
  var hadError = false

  private def cloneChoiceSymbol(symbol: Symbol): Unit = {
    val newSymbol = symbol match {
      case s: TermSymbol => cc.newSymbolLike(s)
      case s: TypeSymbol => cc.newSymbolLike(s)
      case _             => unreachable
    }
    newSymbol.kind = TypeChoice(symbol.kind.asChoice.symbols map { symbolMap(_) })
    symbolMap(symbol) = newSymbol
  }

  private[this] object TypeSpecializeParam extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = kind match {
      case TypeChoice(choices) => TypeChoice(choices map symbolMap)
      case _                   => super.transform(kind)
    }
  }

  private def skipIt(entity: Entity): Boolean = {
    // If the root entity does not have any parameters then we just need to
    // type check the const declarations and definitions as these need to be
    // well formed, but otherwise we need not re-write the entity
    entity.declarations forall {
      case Decl(symbol, _) => !symbol.kind.isParam
      case _               => true
    } tap {
      case false => ()
      case true =>
        entity.body foreach {
          case EntDecl(decl @ Decl(symbol, _)) if symbol.kind.isConst =>
            decl rewrite typer
            hadError |= decl.tpe.isError
          case EntDefn(defn) =>
            defn rewrite typer
            hadError |= defn.tpe.isError
          case _ =>
        }
    }
  }

  override def skip(tree: Tree): Boolean = hadError || entityLevel == 0 && {
    tree match {
      case Root(_, entity) => skipIt(entity)
      case entity: Entity  => skipIt(entity)
      case _               => false
    }
  }

  private def cloneForwardSymbols(trees: List[Tree]): Unit =
    trees foreach {
      case EntFunction(Sym(symbol: TermSymbol, _), _) =>
        symbolMap(symbol) = cc.newSymbolLike(symbol)
      case EntInstance(Sym(symbol: TermSymbol, _), _, _, _) =>
        symbolMap(symbol) = cc.newSymbolLike(symbol)
      case EntEntity(Entity(Sym(symbol: TypeSymbol, _), _)) =>
        symbolMap(symbol) = cc.newSymbolLike(symbol)
      case EntGen(GenIf(_, thenItems, elseItems)) =>
        cloneForwardSymbols(thenItems)
        cloneForwardSymbols(elseItems)
      case EntGen(GenFor(_, _, _, body)) =>
        cloneForwardSymbols(body)
      case EntGen(GenRange(_, _, _, body)) =>
        cloneForwardSymbols(body)
      case GenIf(_, thenItems, elseItems) =>
        cloneForwardSymbols(thenItems)
        cloneForwardSymbols(elseItems)
      case GenFor(_, _, _, body) =>
        cloneForwardSymbols(body)
      case GenRange(_, _, _, body) =>
        cloneForwardSymbols(body)
      case _ =>
    }

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity =>
      entityLevel += 1
      // Clone new functions, instances and nested entities up front,
      // as they can be referenced before definition
      cloneForwardSymbols(entity.body)

    case _: EntDecl =>
    case _: EntDefn =>
    // Any other ent means there won't be any more param/const decls later
    case _: Ent => checkDefn = false

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

    case Decl(symbol, init) if entityLevel == 1 && symbol.kind.isParam =>
      // Specialize the parameter type itself
      val TypeParam(kind) = symbol.kind.asParam
      val newKind = TypeParam(kind rewrite TypeSpecializeParam)

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
      newSymbol.attr update symbol.attr
      symbolMap(symbol) = newSymbol

      // Remember symbols we replaced
      outerParams add symbol

      // Create new decl
      val newDecl = Decl(newSymbol, newInit) withLoc tree.loc

      // Type check the new declaration and hence the parameter assignment.
      newDecl rewrite typer

      // Latch error
      hadError |= newDecl.tpe.isError

      // Now change the symbol into a constant
      newSymbol.kind = TypeConst(newKind.kind)

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

    ////////////////////////////////////////////////////////////////////////////
    // Clone other declaration symbols
    ////////////////////////////////////////////////////////////////////////////

    case decl: Declaration =>
      def cloneSymbol(symbol: TermSymbol): TermSymbol = {
        // Rewrite references in types
        val newKind = symbol.kind rewrite TypeSpecializeParam
        // Clone the symbol
        cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      } tap { newSymbol =>
        newSymbol.attr update symbol.attr
        symbolMap(symbol) = newSymbol
      }
      val (newDecl, newSymbol) = decl match {
        case DeclRef(sym @ Sym(symbol: TermSymbol, _), _, init) =>
          val newSymbol = cloneSymbol(symbol)
          val newSym = sym.copy(symbol = newSymbol) withLoc sym.loc
          val newDecl = DeclRef(newSym, newSymbol.kind, init) withLoc tree.loc
          (newDecl, newSymbol)
        case _: DeclRef => unreachable
        case Decl(symbol, init) =>
          val newSymbol = cloneSymbol(symbol)
          val newDecl = Decl(newSymbol, init) withLoc tree.loc
          (newDecl, newSymbol)
      }
      // Type check it if it's a const declaration in the outermost entity
      if (entityLevel == 1 && newSymbol.kind.isConst) {
        newDecl rewrite typer
        // Latch error
        hadError |= newDecl.tpe.isError
      }
      // Done
      newDecl

    ////////////////////////////////////////////////////////////////////////////
    // Clone definition symbols
    ////////////////////////////////////////////////////////////////////////////

    case defn: Definition =>
      def cloneSymbol(symbol: TypeSymbol): TypeSymbol = {
        // Rewrite references in types
        val newKind = symbol.kind rewrite TypeSpecializeParam
        // Clone the symbol
        cc.newTypeSymbol(symbol.name, symbol.loc, newKind)
      } tap { newSymbol =>
        newSymbol.attr update symbol.attr
        symbolMap(symbol) = newSymbol
      }
      val newDefn = defn match {
        case DefnRef(sym @ Sym(symbol: TypeSymbol, _), _) =>
          val newSymbol = cloneSymbol(symbol)
          val newSym = sym.copy(symbol = newSymbol) withLoc sym.loc
          DefnRef(newSym, newSymbol.kind) withLoc tree.loc
        case _: DefnRef => unreachable
        case Defn(symbol) =>
          val newSymbol = cloneSymbol(symbol)
          Defn(newSymbol) withLoc tree.loc
      }
      // Type check it if required
      if (checkDefn) {
        newDefn rewrite typer
        // Latch error
        hadError |= newDefn.tpe.isError
      }
      // Done
      newDefn

    ////////////////////////////////////////////////////////////////////////////
    // Update references
    ////////////////////////////////////////////////////////////////////////////

    case ExprSym(symbol) =>
      if (symbol.kind.isChoice && !(symbolMap contains symbol)) {
        // Clone choice symbol on first reference
        cloneChoiceSymbol(symbol)
      }

      symbolMap.get(symbol) map {
        ExprSym(_) withLoc tree.loc
      } getOrElse tree

    case Sym(symbol, idxs) =>
      if (symbol.kind.isChoice && !(symbolMap contains symbol)) {
        // Clone choice symbol on first reference
        cloneChoiceSymbol(symbol)
      }

      symbolMap.get(symbol) map {
        Sym(_, idxs) withLoc tree.loc
      } getOrElse tree

    ////////////////////////////////////////////////////////////////////////////
    // Update instances
    ////////////////////////////////////////////////////////////////////////////

    case EntInstance(Sym(iSymbol, Nil), Sym(eSymbol, Nil), _, _) =>
      // The 2 Sym instances have already been rewritten, we only need to
      // update the type of the iSymbol to refer to cloned entities
      symbolMap get eSymbol foreach {
        case nSymbol: TypeSymbol => iSymbol.kind = TypeInstance(nSymbol)
        case _                   => unreachable
      }

      // No need to re-write
      tree

    ////////////////////////////////////////////////////////////////////////////
    // Clone nested entities/Create the specialized entity
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      if (entityLevel > 1) {
        // Update type of entity to refer to the cloned symbols
        entity.symbol.kind = entity.typeBasedOnContents

        // Symbol already cloned in enter, no need to do anything here
        entity
      } else {
        // Compute name of new entity
        val newName = entity.symbol.name + cc.sep + {
          paramValues.toList map { case (p, v) => s"${p}_${v}" } mkString cc.sep
        }

        // Clone root entity symbol
        val newSymbol = cc.newTypeSymbol(newName, entity.symbol.loc, entity.typeBasedOnContents)
        newSymbol.attr update entity.symbol.attr

        // Build new node
        Entity(Sym(newSymbol, Nil) withLoc newSymbol.loc, entity.body) withLoc entity.loc
      }
    } tap { _ =>
      entityLevel -= 1
    }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    if (!hadError) {
      assert(entityLevel == 0)

      tree visitAll {
        case node @ ExprSym(symbol: TermSymbol) if outerParams contains symbol =>
          cc.ice(node, "Reference to parameter remains")
        case node @ Sym(symbol: TermSymbol, _) if outerParams contains symbol =>
          cc.ice(node, "Sym to parameter remains")
        case node @ Decl(symbol, _) if outerParams contains symbol =>
          cc.ice(node, "Outer parameter declaration remains")
      }
    }
  }
}
