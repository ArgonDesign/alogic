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
// Specialize a tree such that all params are replaced with consts, either
// initialized to the expression given in bindings, or to the original
// default value of the param.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class InlineParam(bindings: Map[TermSymbol, Expr])(implicit cc: CompilerContext)
    extends TreeTransformer {

  // Map from original parameter symbol to the new const symbol
  private[this] val symbolMap = mutable.Map[TermSymbol, TermSymbol]()

  private[this] object TypeSpecializeParams extends TreeInTypeTransformer(this)

  override def enter(tree: Tree): Unit = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Leave parameters be, we will change them in transform
    ////////////////////////////////////////////////////////////////////////////

    case Decl(_, _: TypeParam, _) => ()

    ////////////////////////////////////////////////////////////////////////////
    // Clone the symbols of any other declaration
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // Allocate new functions up front, as they can be referenced before definition
      for (Function(Sym(symbol: TermSymbol), _) <- entity.functions) {
        // Create the new symbol
        val newSymbol = cc.newSymbolLike(symbol)
        symbolMap(symbol) = newSymbol
      }
    }

    case decl @ Decl(Sym(symbol: TermSymbol), kind, _) => {
      // Create the new symbol
      val newKind = kind rewrite TypeSpecializeParams
      val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      newSymbol.attr update symbol.attr
      symbolMap(symbol) = newSymbol
    }

    case inst @ Instance(Sym(symbol: TermSymbol), _, _, _) => {
      // Create the new symbol
      val newSymbol = cc.newSymbolLike(symbol)
      symbolMap(symbol) = newSymbol
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Update references
    ////////////////////////////////////////////////////////////////////////////

    case Sym(symbol: TermSymbol) => {
      symbolMap.get(symbol) map { Sym(_) regularize tree.loc } getOrElse tree
    }

    ////////////////////////////////////////////////////////////////////////////
    // Change param declarations to const declarations and update initializer
    ////////////////////////////////////////////////////////////////////////////

    case Decl(Sym(symbol: TermSymbol), TypeParam(kind), default) => {
      // Create the new symbol
      val newKind = TypeConst(kind rewrite TypeSpecializeParams)
      val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
      symbolMap(symbol) = newSymbol
      // Use the initializer form the bindings, or the default
      val init = bindings.get(symbol) orElse default
      // Create new declaration
      Decl(Sym(newSymbol), newSymbol.denot.kind, init) regularize tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Ensure other Delc are consistent, as we have rewritten kind in enter
    ////////////////////////////////////////////////////////////////////////////

    case decl @ Decl(Sym(symbol: TermSymbol), kind, _) if kind ne symbol.denot.kind => {
      // Update declaration
      decl.copy(kind = symbol.denot.kind) regularize tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Create the new entity
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // Create new entity
      val newName = if (bindings.nonEmpty) {
        val parts = for ((symbol, expr) <- bindings) yield {
          s"${symbol.name}_${expr.value.get}"
        }
        s"${entitySymbol.name}__${parts mkString "__"}"
      } else {
        entitySymbol.name
      }

      val oldKind = entitySymbol.denot.kind.asInstanceOf[TypeEntity]
      val newKind = TypeEntity(newName, oldKind.portSymbols, Nil)
      val newSymbol = cc.newTypeSymbol(newName, tree.loc, newKind)
      newSymbol.attr update entitySymbol.attr

      val sym = TypeAssigner(Sym(newSymbol) withLoc tree.loc)

      TypeAssigner(entity.copy(ref = sym) withVariant entity.variant withLoc tree.loc)
    }

    case _ => tree
  }
}
