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
// Checks for unused variables
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class UnusedCheck(implicit cc: CompilerContext) extends TreeTransformer { namer =>

  override val typed = false

  // Set of declared symbols, keyed by entity containing the definition
  private val declared =
    mutable.Map[Option[TypeSymbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Set of referenced symbols, keyed by entity containing the reference
  private val used =
    mutable.Map[Option[TypeSymbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Node counter
  private var count = 0

  // Entity symbol stack
  private val eSymbolStack = mutable.Stack[Option[TypeSymbol]](None)

  // Flag to indicate we are in a verbatim entity
  private var inVerbatimEntity = false

  private def markDecl(symbol: Symbol): Unit = declared(eSymbolStack.top) add symbol

  private def markUsed(symbol: Symbol): Unit = used(eSymbolStack.top) add symbol

  override def enter(tree: Tree): Unit = {
    count += 1
    tree match {
      case ExprRef(symbol) =>
        // Mark symbol as used
        markUsed(symbol)

      case Decl(symbol, _) =>
        // Add symbol
        markDecl(symbol)
        // Mark all verbatim declarations as used
        if (inVerbatimEntity) {
          markUsed(symbol)
        }

      case EntFunction(Sym(symbol), _) =>
        // Add symbol
        markDecl(symbol)
        // Mark entry functions as used
        if (symbol.attr.entry.isSet) {
          markUsed(symbol)
        }

      case Entity(Sym(symbol: TypeSymbol), _) =>
        // Add symbol
        markDecl(symbol)
        // Mark the root entity as used
        if (eSymbolStack.top.isEmpty) {
          markUsed(symbol)
        }
        // Update state
        declared(Some(symbol)) = mutable.Set()
        used(Some(symbol)) = mutable.Set()
        eSymbolStack push Some(symbol)
        inVerbatimEntity = symbol.attr.variant contains "verbatim"

      case EntInstance(Sym(iSymbol), Sym(eSymbol), _, _) =>
        // Add instance symbol
        markDecl(iSymbol)
        // Mark entity as used
        markUsed(eSymbol)

      case _ =>
    }
  }

  override def transform(tree: Tree): Tree = {
    count -= 1

    tree match {
      case _: Entity =>
        eSymbolStack.pop()
        inVerbatimEntity = false
      case _ =>
    }

    // When we have processed the root node, check references
    if (count == 0) {
      val allDeclared = declared.values.foldLeft(Set.empty[Symbol]) { _ union _ }

      // Mark all instance symbols which are instances of entities which refer
      // to outer pots as used
      for {
        iSymbol <- allDeclared
      } {
        iSymbol.kind match {
          case TypeInstance(eSymbol) =>
            val portSymbols = eSymbol.kind match {
              case TypeEntity(_, pSymbols, _) => pSymbols
              case _                          => unreachable
            }
            val hasExternalPortRef = used(Some(eSymbol)) exists { symbol =>
              (symbol.kind.isIn || symbol.kind.isOut) && !(portSymbols contains symbol)
            }
            if (hasExternalPortRef) {
              markUsed(iSymbol)
            }
          case _ =>
        }
      }

      val allUsed = used.values.foldLeft(Set.empty[Symbol]) { _ union _ }

      for {
        symbol <- allDeclared diff allUsed
        if !(symbol.attr.unused.get contains true)
      } {
        val hint = symbol.kind match {
          case _: TypeArray    => "Array"
          case _: TypeCtrlFunc => "Function"
          case _: TypeCombFunc => "Function"
          case _: TypeEntity   => "Entity"
          case _: TypeIn       => "Input port"
          case _: TypeOut      => "Output port"
          case _: TypeParam    => "Parameter"
          case _: TypeConst    => "Constant"
          case _: TypePipeline => "Pipeline variable"
          case _: TypeInstance => "Instance"
          case _               => "Variable"
        }
        cc.warning(symbol, s"${hint} '${symbol.name}' is unused")
      }
    }

    // Nothing to actually rewrite
    tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(count == 0)
    assert(eSymbolStack.lengthIs == 1)
    assert(eSymbolStack.top.isEmpty)
    assert(!inVerbatimEntity)
  }
}

object UnusedCheck extends TreeTransformerPass {
  val name = "unused-check"
  def create(implicit cc: CompilerContext) = new UnusedCheck
}
