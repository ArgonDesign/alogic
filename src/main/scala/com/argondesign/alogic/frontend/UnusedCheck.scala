////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Checks for unused Symbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class UnusedCheck(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // Set of declared symbols, keyed by entity containing the definition
  private val declared =
    mutable.Map[Option[Symbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Set of referenced symbols, keyed by entity containing the reference
  private val used =
    mutable.Map[Option[Symbol], mutable.Set[Symbol]](None -> mutable.Set())

  // Set of symbols which contain external references
  private val hasExtRefs = mutable.Set[Symbol]()

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
    declared(Some(symbol)) = mutable.Set()
    used(Some(symbol)) = mutable.Set()
    symbolStack push symbol
    inVerbatimEntity = variant == EntityVariant.Ver
    // Mark definitions up front so nested entities can check external refs
    body foreach {
      case EntSplice(desc: Desc) => markDecl(desc.symbol)
      case _                     =>
    }
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case ExprSym(symbol) =>
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
        // Mark symbols used during elaboration only
        if (desc.symbol.attr.wasUsed contains true) {
          markUsed(desc.symbol)
        }
        // Mark parametrized symbols that have been specialzied at least once
        if (desc.symbol.attr.specializations.isSet) {
          markUsed(desc.symbol)
        }
        // Behaviour specific to certain kinds of definitions
        desc match {
          case csomag: DescPackage =>
            // Mark the package symbol used
            markUsed(desc.symbol)
            // Mark all members as used
            csomag.descs foreach { desc => markUsed(desc.symbol) }
          case DescEntity(_, _, variant, body) =>
            processEntity(desc.symbol, variant, body)
          case DescSingleton(_, _, variant, body) =>
            processEntity(desc.symbol, variant, body)
          case record: DescRecord =>
            // Mark all members as used
            record.descs foreach { desc =>
              markUsed(desc.symbol)
            }
          case DescFunc(_, _, variant, _, args, _) =>
            // Mark entry point functions as used
            if (desc.symbol.attr.entry contains true) {
              markUsed(desc.symbol)
            }
            // Mark foreign function arguments as used
            if (variant == FuncVariant.Xeno) {
              args foreach { desc => markUsed(desc.symbol) }
            }
          case _ =>
        }

      case _ =>
    }
    tree match {
      case _: DescParametrized => Some(tree) // Stop descent
      case _                   => None
    }
  }

  override def transform(tree: Tree): Tree = tree tap {
    case _: DescEntity =>
      symbolStack.pop()
      inVerbatimEntity = false
    case desc: DescSingleton =>
      symbolStack.pop()
      inVerbatimEntity = false
      // Mark this instance as used if it has external references. This is
      // to ensure we don't warn for singletons which deal directly with
      // enclosing ports and have no explicit ports of their own.
      if (hasExtRefs(desc.symbol)) {
        markUsed(desc.symbol)
      }
    case desc: DescInstance =>
      // Similarly, mark used if the instantiated entity has external refs
      if (hasExtRefs(desc.symbol.kind.asEntity.symbol)) {
        markUsed(desc.symbol)
      }
    case _ =>
  }

  override protected def finish(tree: Tree): Tree = tree tap { _ =>
    // When we have processed the root node, check references
    val allDeclared = declared.valuesIterator.foldLeft(Set.empty[Symbol])(_ union _)

    val allUsed = used.valuesIterator.foldLeft(Set.empty[Symbol])(_ union _)

    for {
      symbol <- (allDeclared diff allUsed).toSeq.distinctBy(_.loc).sorted
    } {
      def hint(desc: Desc): String = desc match {
        case _: DescVar | _: DescVal | _: DescStatic => "Variable"
        case _: DescIn                               => "Input port"
        case _: DescOut                              => "Output port"
        case _: DescPipeVar                          => "Pipeline variable"
        case _: DescPipeIn                           => "Pipeline input port"
        case _: DescPipeOut                          => "Pipeline output port"
        case _: DescParam                            => "Parameter"
        case _: DescParamType                        => "Type parameter"
        case _: DescConst                            => "Constant"
        case _: DescArray                            => "Array"
        case _: DescSram                             => "SRAM"
        case _: DescType                             => "Type"
        case _: DescEntity                           => "Entity"
        case _: DescRecord                           => "struct"
        case _: DescInstance                         => "Instance"
        case _: DescSingleton                        => "Singleton instance"
        case _: DescFunc                             => "Function"
        case _: DescGenVar                           => unreachable // Evaluated during Elaborate
        case _: DescGenIf                            => unreachable // Removes by Elaborate
        case _: DescGenFor                           => unreachable // Removes by Elaborate
        case _: DescGenRange                         => unreachable // Removes by Elaborate
        case _: DescGenScope                         => unreachable // Removed by Finalize
        case _: DescPackage                          => unreachable // Always used
        case DescParametrized(_, _, desc: Desc, _)   => s"Parametrized ${hint(desc)}"
        case _: DescAlias                            => unreachable // Removed by Finalize
      }
      cc.warning(symbol, s"${hint(symbol.desc)} '${symbol.origName}' is unused")
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(symbolStack.isEmpty)
    assert(!inVerbatimEntity)
  }

}

object UnusedCheck {
  def apply(desc: Desc)(implicit cc: CompilerContext): Unit = desc rewrite new UnusedCheck
}
