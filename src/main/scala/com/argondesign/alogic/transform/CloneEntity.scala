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
// Clone an entity that is identical in structure but uses newly allocated
// symbols. Any parameters that are provided in parameterBindings are converted
// to constants with the given value.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.PartialMatch

import scala.collection.immutable.ListMap
import scala.collection.mutable

final class CloneEntity(parameterBindings: ListMap[TermSymbol, Expr])(implicit cc: CompilerContext)
    extends TreeTransformer
    with PartialMatch {

  // Map from original symbol to the new symbol
  private[this] val symbolMap = mutable.Map[TermSymbol, TermSymbol]()

  private[this] object TypeCloneEntity extends TreeInTypeTransformer(this)

  override def enter(tree: Tree): Unit = tree match {

    case entity: Entity => {
      // Clone new functions up front, as they can be referenced before definition
      for (Function(Sym(symbol: TermSymbol), _) <- entity.functions) {
        symbolMap(symbol) = cc.newSymbolLike(symbol)
      }
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Clone the symbols in declaration
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol, init) => {
      // Change parameters that have a binding into constants
      val (newKind, newInit) = parameterBindings.get(symbol) map { expr =>
        val TypeParam(kind) = symbol.denot.kind
        val newKind: Type = TypeConst(kind rewrite TypeCloneEntity)
        val newInit: Option[Expr] = Some(expr)
        (newKind, newInit)
      } getOrElse {
        val kind = symbol.denot.kind rewrite TypeCloneEntity
        (kind, init)
      }
      // Clone the symbol
      val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      newSymbol.attr update symbol.attr
      symbolMap(symbol) = newSymbol
      // Update the constValue if its a constant
      newKind match {
        case _: TypeConst => newSymbol.attr.constValue set newInit.get
        case _            => ()
      }
      // Rewrite with new symbol and init
      Decl(newSymbol, newInit) regularize tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone instance symbols
    ////////////////////////////////////////////////////////////////////////////

    case inst @ Instance(Sym(symbol: TermSymbol), _, _, _) => {
      // Clone the symbol
      val newSymbol = cc.newSymbolLike(symbol)
      symbolMap(symbol) = newSymbol
      // Rewrite with new symbol
      inst.copy(ref = Sym(newSymbol)) regularize tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Update references
    ////////////////////////////////////////////////////////////////////////////

    case Sym(symbol: TermSymbol) => {
      symbolMap.get(symbol) map { Sym(_) regularize tree.loc } getOrElse tree
    }

    ////////////////////////////////////////////////////////////////////////////
    // Create the new entity
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      val newName = {
        val suffixes = for ((symbol, expr) <- parameterBindings) yield {
          s"${symbol.name}_${expr.value.get}"
        }

        (entitySymbol.name :: suffixes.toList) mkString cc.sep
      }

      val newKind = {
        val portSymbols = for {
          Decl(symbol, _) <- entity.declarations
          if symbol.denot.kind.isInstanceOf[TypeIn] || symbol.denot.kind.isInstanceOf[TypeOut]
        } yield {
          symbol
        }

        val paramSymbols = for {
          Decl(symbol, _) <- entity.declarations
          if symbol.denot.kind.isInstanceOf[TypeParam]
        } yield {
          symbol
        }

        TypeEntity(newName, portSymbols, paramSymbols)
      }

      val newSymbol = cc.newTypeSymbol(newName, tree.loc, newKind)
      newSymbol.attr update entitySymbol.attr

      val sym = TypeAssigner(Sym(newSymbol) withLoc tree.loc)
      TypeAssigner(entity.copy(ref = sym) withVariant entity.variant withLoc tree.loc)

      // TODO: Apply Typer to specialized result (iff parameterBindings.nonEmpty)
    }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visitAll {
      case decl @ Decl(symbol, Some(init)) => {
        symbol.denot.kind partialMatch {
          case _: TypeConst if symbol.attr.constValue.value != init => {
            cc.ice(decl, "Const symbol with wrong constValue")
          }
        }
      }
    }
  }
}
