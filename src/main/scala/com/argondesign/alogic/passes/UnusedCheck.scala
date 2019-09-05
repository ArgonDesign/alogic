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
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class UnusedCheck(implicit cc: CompilerContext) extends TreeTransformer {

  override val typed = false

  // Set of declared symbols, keyed by entity containing the definition
  private val declared =
    mutable.Map[Option[TypeSymbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Set of referenced symbols, keyed by entity containing the reference
  private val used =
    mutable.Map[Option[TypeSymbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Node counter
  private var count = 0

  // Root node file name
  private var rootFileName: String = _

  // Entity symbol stack
  private val eSymbolStack = mutable.Stack[TypeSymbol]()

  // Flag to indicate we are in a verbatim entity
  private var inVerbatimEntity = false

  private def markDecl(symbol: Symbol): Unit = declared(eSymbolStack.headOption) add symbol

  private def markUsed(symbol: Symbol): Unit = used(eSymbolStack.headOption) add symbol

  private object TypeUnusedCheck extends TreeInTypeTransformer(this) {
    override protected def enter(kind: Type): Unit = kind match {
      case TypeRef(Sym(symbol, _)) =>
        // Mark symbol as used
        markUsed(symbol)
        // Check choice symbols at use site
        if (symbol.kind.isChoice) {
          walk(symbol.kind)
        }
      case TypeChoice(symbols) => symbols foreach markUsed
      case _                   =>
    }
  }

  override def enter(tree: Tree): Unit = {
    if (count == 0) {
      rootFileName = tree.loc.source.name
    }
    count += 1
    tree match {
      case ExprSym(symbol) =>
        // Check choice symbols at use site
        if (symbol.kind.isChoice) {
          TypeUnusedCheck(symbol.kind)
        }
        // Mark symbol as used
        markUsed(symbol)

      case ExprRef(Sym(symbol, _)) =>
        // Check choice symbols at use site
        if (symbol.kind.isChoice) {
          TypeUnusedCheck(symbol.kind)
        }
        // Mark symbol as used
        markUsed(symbol)

      case Decl(symbol, _) =>
        // Walk type
        TypeUnusedCheck(symbol.kind)
        // Add symbol
        markDecl(symbol)
        // Mark all verbatim declarations as used
        if (inVerbatimEntity) {
          markUsed(symbol)
        }

      case DeclRef(Sym(symbol, _), _, _) =>
        // Walk type
        TypeUnusedCheck(symbol.kind)
        // Add symbol
        markDecl(symbol)
        // Mark all verbatim declarations as used
        if (inVerbatimEntity) {
          markUsed(symbol)
        }

      case Defn(symbol) =>
        // Walk type
        TypeUnusedCheck(symbol.kind)
        // Add symbol
        markDecl(symbol)
        // Mark externally defined types as used as well
        if (symbol.loc.file != rootFileName) {
          markUsed(symbol)
        }

      case DefnRef(Sym(symbol, _), _) =>
        // Walk type
        TypeUnusedCheck(symbol.kind)
        // Add symbol
        markDecl(symbol)

      case EntFunction(Sym(symbol, _), _) =>
        // Add symbol
        markDecl(symbol)
        // Mark entry functions as used
        if (symbol.attr.entry.isSet) {
          markUsed(symbol)
        }

      case Entity(Sym(symbol: TypeSymbol, _), _) =>
        // Add symbol
        markDecl(symbol)
        // Mark the root entity as used
        if (eSymbolStack.isEmpty) {
          markUsed(symbol)
        }
        // Update state
        declared(Some(symbol)) = mutable.Set()
        used(Some(symbol)) = mutable.Set()
        eSymbolStack push symbol
        inVerbatimEntity = symbol.attr.variant contains "verbatim"

      case EntInstance(Sym(iSymbol, _), Sym(eSymbol, _), _, _) =>
        // Check choice symbols at use site
        if (eSymbol.kind.isChoice) {
          TypeUnusedCheck(eSymbol.kind)
        }
        // Add instance symbol
        markDecl(iSymbol)
        // Mark entity as used
        markUsed(eSymbol)

      case GenRange(Decl(symbol, _), _, _, _) =>
        // Mark loop variable as used
        markUsed(symbol)

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
          case TypeInstance(symbol) =>
            val hasExternalPortRef = {
              val eSymbols = symbol.kind match {
                case _: TypeEntity => Iterator(symbol.asInstanceOf[TypeSymbol])
                case TypeChoice(choices) =>
                  assert(choices forall { _.kind.isEntity })
                  choices.iterator map { _.asInstanceOf[TypeSymbol] }
                case _ => unreachable
              }
              eSymbols exists { eSymbol =>
                used.get(Some(eSymbol)) match {
                  case Some(set) =>
                    val portSymbols = eSymbol.kind.asEntity.portSymbols
                    set exists { symbol =>
                      (symbol.kind.isIn || symbol.kind.isOut) && !(portSymbols contains symbol)
                    }
                  case None => false
                }
              }
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
        val hint = (symbol.isTermSymbol, symbol.kind) match {
          case (_, _: TypeArray)      => "Array"
          case (_, _: TypeCtrlFunc)   => "Function"
          case (_, _: TypeCombFunc)   => "Function"
          case (_, _: TypeEntity)     => "Entity"
          case (_, _: TypeIn)         => "Input port"
          case (_, _: TypeOut)        => "Output port"
          case (_, _: TypeParam)      => "Parameter"
          case (_, _: TypeConst)      => "Constant"
          case (_, _: TypePipeline)   => "Pipeline variable"
          case (_, _: TypeInstance)   => "Instance"
          case (true, _)              => "Variable"
          case (false, _: TypeStruct) => "struct"
          case (false, _)             => "Type"
        }
        cc.warning(symbol, s"${hint} '${symbol.name}' is unused")
      }
    }

    // Nothing to actually rewrite
    tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(count == 0)
    assert(eSymbolStack.isEmpty)
    assert(!inVerbatimEntity)
  }
}

object UnusedCheck extends TreeTransformerPass {
  val name = "unused-check"
  def create(implicit cc: CompilerContext) = new UnusedCheck
}
