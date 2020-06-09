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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class UnusedCheck(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  override val typed = false

  // Set of declared symbols, keyed by entity containing the definition
  private val declared =
    mutable.Map[Option[Symbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Set of referenced symbols, keyed by entity containing the reference
  private val used =
    mutable.Map[Option[Symbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Set of symbols which contain external references
  private val hasExtRefs = mutable.Set[Symbol]()

  // Node counter
  private var count = 0

  // Root node file name
  private var rootFileName: String = _

  // Declaration symbol stack
  private val symbolStack = mutable.Stack[Symbol]()

  // Flag to indicate we are in a verbatim entity
  private var inVerbatimEntity = false

  private def markDecl(symbol: Symbol): Unit = declared(symbolStack.headOption) add symbol

  private def markUsed(symbol: Symbol): Unit = used(symbolStack.headOption) add symbol

  private def checkExtRef(symbol: Symbol): Unit = {
    // Only need to do this if we haven't found an external reference yet.
    symbolStack.iterator.takeWhile { s =>
      !(declared(Some(s)) contains symbol)
    } foreach hasExtRefs.add
  }

  private def processEntity(symbol: Symbol, variant: EntityVariant.Type, body: List[Ent]): Unit = {
    // Mark the root entity as used
    if (symbolStack.isEmpty) {
      markUsed(symbol)
    }
    declared(Some(symbol)) = mutable.Set()
    used(Some(symbol)) = mutable.Set()
    symbolStack push symbol
    inVerbatimEntity = variant == EntityVariant.Ver
    // Mark definitions up front so nested entities can check external refs
    body foreach {
      case EntDesc(desc) => markDecl(desc.symbol)
      case _             =>
    }
  }

  override def enter(tree: Tree): Option[Tree] = {
    if (count == 0) {
      rootFileName = tree.loc.source.name
    }
    count += 1
    tree match {
      case ExprSym(symbol) =>
        // Mark symbol as used
        markUsed(symbol)
        // Check if this is an external reference
        checkExtRef(symbol)

      case ExprRef(Sym(symbol, _)) =>
        // Mark symbol as used
        markUsed(symbol)
        // Check if this is an external reference
        checkExtRef(symbol)

      case desc: Desc =>
        // Add symbol
        markDecl(desc.symbol)
        // Mark all verbatim declarations as used
        if (inVerbatimEntity) {
          markUsed(desc.symbol)
        }
        // Mark externally defined types as used
        if (desc.symbol.loc.file != rootFileName) {
          markUsed(desc.symbol)
        }
        // Behaviour specific to certain kinds of definitions
        desc match {
          case DescEntity(_, variant, body) =>
            processEntity(desc.symbol, variant, body)
          case DescSingleton(_, variant, body) =>
            processEntity(desc.symbol, variant, body)
          case record: DescRecord =>
            // Mark all members as used
            record.descs foreach { desc =>
              markUsed(desc.symbol)
            }
          case DescFunc(_, variant, _, args, _) =>
            // Mark entry point functions as used
            if (desc.symbol.attr.entry contains true) {
              markUsed(desc.symbol)
            }
            // Mark foreign function arguments as used
            if (variant == FuncVariant.Xeno) {
              args foreach { desc =>
                markUsed(desc.symbol)
              }
            }
          case _: DescChoice =>
            // Assume used
            markUsed(desc.symbol)
          case _ =>
        }

      case GenRange(List(StmtDesc(DescGen(Sym(symbol, _), _, _))), _, _, _) =>
        // Mark loop variable as used
        markUsed(symbol)

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = {
    count -= 1

    tree match {
      case _: DescEntity =>
        symbolStack.pop()
        inVerbatimEntity = false
      case desc: DescSingleton =>
        symbolStack.pop()
        inVerbatimEntity = false
        // Mark this instance as used if it has external references
        if (hasExtRefs(desc.symbol)) {
          markUsed(desc.symbol)
        }
      case desc: DescInstance =>
        // We don't know the type of this yet, assume used
        markUsed(desc.symbol)
      case _ =>
    }

    // When we have processed the root node, check references
    if (count == 0) {
      val allDeclared = declared.values.foldLeft(Set.empty[Symbol])(_ union _)

      val allUsed = used.values.foldLeft(Set.empty[Symbol])(_ union _)

      for {
        symbol <- allDeclared diff allUsed
        if !(symbol.attr.unused.get contains true)
      } {
        val hint = symbol.desc match {
          case _: DescVar       => "Variable"
          case _: DescIn        => "Input port"
          case _: DescOut       => "Output port"
          case _: DescPipeline  => "Pipeline variable"
          case _: DescParam     => "Parameter"
          case _: DescParamType => "Type parameter"
          case _: DescConst     => "Constant"
          case _: DescGen       => "'gen' variable"
          case _: DescArray     => "Array"
          case _: DescSram      => "SRAM"
          case _: DescType      => "Type"
          case _: DescEntity    => "Entity"
          case _: DescRecord    => "struct"
          case _: DescInstance  => "Instance"
          case _: DescSingleton => "Singleton instance"
          case _: DescFunc      => "Function"
          case _                => unreachable
        }
        val name = symbol.attr.sourceName.get match {
          case None               => symbol.name
          case Some((base, idxs)) => idxs mkString (s"$base#[", ", ", "]")
        }
        cc.warning(symbol, s"$hint '$name' is unused")
      }
    }

    // Nothing to actually rewrite
    tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(count == 0)
    assert(symbolStack.isEmpty)
    assert(!inVerbatimEntity)
  }

}

object UnusedCheck extends PreElaboratePass {
  val name = "unused-check"
  def create(implicit cc: CompilerContext) = new UnusedCheck
}
