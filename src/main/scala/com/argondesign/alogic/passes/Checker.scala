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
// The checker runs immediately after the builder and checks that input that
// is although valid according to the input grammar, is also valid in the
// context it is used in. The job of the checker is to ensure that assumptions
// made by later stages hold for the current input. The only rewrites the
// checker is allowed to do are:
// - Remove entire offending subtrees where such removal yields a valid tree
//   enabling forward progress
// - Replace illegal subtrees with alternatives where such a fix yields a valid
//   tree enabling forward progress
// - Replace offending subtrees with Error nodes
// The checker should always signal an error when a rewrite is done, and not
// rewrite any nodes which are not erroneous. Then trying to fix the tree, the
// checker should apply the 'most correct' fix that would enable the compiler
// to proceed as far as possible in order to allow reporting of further issues.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.enums.EntityVariant

import scala.collection.mutable

final class Checker(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // TODO: Error for param in Singleton

  override val typed: Boolean = false

  private[this] val variantStack = mutable.Stack[EntityVariant.Type]()

  private[this] val singletonStack = mutable.Stack[Boolean]()

  private[this] var loopLevel: Int = 0

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case DescEntity(ident: Ident, variant, body) =>
        variantStack push variant
        singletonStack push false
        checkEntityBody(ident, variant, body)
      case DescSingleton(ident: Ident, variant, body) =>
        variantStack push variant
        singletonStack push true
        checkEntityBody(ident, variant, body)
      case _: StmtLoop | _: StmtWhile | _: StmtFor | _: StmtDo =>
        loopLevel += 1
      case _ =>
    }
    None
  }

  private def entErr(node: Tree, content: String) = {
    val variant = variantStack.top match {
      case EntityVariant.Fsm => "fsm"
      case EntityVariant.Net => "network"
      case EntityVariant.Ver => "verbatim entity"
    }
    cc.error(node, s"'$variant' cannot contain $content")
    Stump
  }

  private def clsErr(node: Tree, content: String) = {
    cc.error(node, s"'struct' cannot contain $content")
    Stump
  }

  private def notVariant(variant: EntityVariant.Type) =
    variantStack.nonEmpty && variantStack.top != variant

  private def checkEntityBody(ident: Ident, variant: EntityVariant.Type, body: List[Ent]): Unit = {
    val combProcesses = body collect { case node: EntCombProcess => node }

    if (combProcesses.lengthIs > 1) {
      combProcesses foreach {
        cc.error(_, s"Multiple fence blocks specified in entity '${ident.name}'")
      }
    }

    val verbatims = body collect { case node: EntVerbatim => node }

    val nonports = body filter {
      case EntDesc(_: DescIn)    => false
      case EntDesc(_: DescOut)   => false
      case EntDesc(_: DescParam) => false
      case EntDesc(_: DescConst) => false
      case _                     => true
    }

    if (variant != EntityVariant.Ver && verbatims.nonEmpty && verbatims.length == nonports.length) {
      cc.warning(
        ident,
        s"Entity '${ident.name}' contains only verbatim blocks, use a 'verbatim entity' instead")
    }
  }

  override def transform(tree: Tree): Tree = tree match {

    case node: EntConnect if notVariant(EntityVariant.Net) => entErr(node, "connections")

    case node: EntCombProcess if notVariant(EntityVariant.Fsm) => entErr(node, "fence blocks")

    case EntDesc(_: DescParam) if singletonStack.top =>
      cc.error(tree, "Singleton entity cannot have parameters. Use a 'const' declaration instead.")
      Stump

    case EntDesc(desc) =>
      variantStack.top match {
        case EntityVariant.Fsm =>
          desc match {
            case _: DescPipeline  => entErr(desc, "pipeline variable declarations")
            case _: DescEntity    => entErr(desc, "nested entities")
            case _: DescInstance  => entErr(desc, "instantiations")
            case _: DescSingleton => entErr(desc, "singleton entities")
            case _                => tree
          }
        case EntityVariant.Net =>
          desc match {
            case _: DescVar   => entErr(desc, "variable declarations")
            case _: DescArray => entErr(desc, "distributed memory declarations")
            case _: DescSram  => entErr(desc, "SRAM declarations")
            case _: DescFunc  => entErr(desc, "function definitions")
            case _            => tree
          }
        case EntityVariant.Ver =>
          desc match {
            case _: DescVar      => entErr(desc, "variable declarations")
            case _: DescPipeline => entErr(desc, "pipeline variable declarations")
            case _: DescArray    => entErr(desc, "distributed memory declarations")
            case DescSram(_, _, _, st) if st != StorageTypeWire =>
              entErr(desc, "registered SRAM declarations")
            case _: DescEntity    => entErr(desc, "nested entities")
            case _: DescInstance  => entErr(desc, "instantiations")
            case _: DescSingleton => entErr(desc, "singleton entities")
            case _: DescFunc      => entErr(desc, "function definitions")
            case _                => tree
          }
      }

    case _: DescEntity =>
      tree tap { _ =>
        variantStack.pop()
        singletonStack.pop()
      }

    case _: DescSingleton =>
      tree tap { _ =>
        variantStack.pop()
        singletonStack.pop()
      }

    case RecDesc(desc) =>
      desc match {
        case _: DescIn       => clsErr(desc, "port declarations")
        case _: DescOut      => clsErr(desc, "port declarations")
        case _: DescPipeline => clsErr(desc, "pipeline declarations")
        case _: DescArray    => clsErr(desc, "distributed memory declarations")
        case _: DescSram     => clsErr(desc, "SRAM declarations")
        case _               => tree
      }

    case desc @ DescOut(_, _, fc, st, None) =>
      val newSt = {
        (fc, st) match {
          case (FlowControlTypeNone, _: StorageTypeSlices) =>
            cc.error(tree,
                     s"Output port '${desc.name}' without flow control cannot use output slices")
            StorageTypeReg
          case (FlowControlTypeValid, _: StorageTypeSlices) =>
            cc.error(
              tree,
              s"Output port '${desc.name}' with 'sync' flow control cannot use output slices")
            StorageTypeReg
          case _ => st
        }
      } tap { st =>
        if ((variantStack.headOption contains EntityVariant.Ver) && st != StorageTypeDefault) {
          cc.error(tree, "'verbatim entity' output ports cannot use a storage specifier")
          StorageTypeDefault
        } else {
          st
        }
      }
      if (newSt eq st) {
        tree
      } else {
        desc.copy(st = newSt) withLoc tree.loc
      }

    case desc @ DescOut(_, _, fc, st, Some(init)) =>
      (fc, st) pipe {
        case (FlowControlTypeNone, StorageTypeWire) => Some("'wire' storage specifier")
        case (FlowControlTypeValid, _)              => Some("'sync' flow control")
        case (FlowControlTypeReady, _)              => Some("'sync ready' flow control")
        case (FlowControlTypeAccept, _)             => Some("'sync accept' flow control")
        case _                                      => None
      } map { hint =>
        cc.error(init, s"Output port with $hint cannot have an initializer")
        desc.copy(initOpt = None) withLoc desc.loc
      } getOrElse tree

    case gen: Gen =>
      gen match {
        case GenFor(inits, _, step, _) =>
          if (step.isEmpty) cc.error(tree, "'gen for' must have at least one step statement")
          if (inits.isEmpty) {
            cc.error(tree, "'gen for' must have at least one declaration initializer")
          }
          tree
        case _ => tree
      }

    case StmtRead() =>
      if (variantStack.lengthIs <= 1) {
        cc.error(tree, "Read statements are only allowed inside nested entities")
        StmtError() withLoc tree.loc
      } else {
        tree
      }

    case StmtWrite() =>
      if (variantStack.lengthIs <= 1) {
        cc.error(tree, "Write statements are only allowed inside nested entities")
        StmtError() withLoc tree.loc
      } else {
        tree
      }

    case StmtDesc(desc) =>
      desc match {
        case _: DescVar => tree
        case _: DescGen => tree
        case _ =>
          cc.error(tree, "Only variables can be declared in declaration statements")
          StmtError() withLoc tree.loc
      }

    case StmtCase(_, cases) =>
      val defaults = cases collect { case c: CaseDefault => c }
      if (defaults.lengthIs <= 1) {
        tree
      } else {
        defaults foreach {
          cc.error(_, "Multiple 'default' clauses specified in case statement")
        }
        StmtError() withLoc tree.loc
      }

    case StmtAssign(lhs, _) =>
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, "Invalid expression on left hand side of '='")
        StmtError() withLoc tree.loc
      }

    case StmtUpdate(lhs, op, _) if !lhs.isLValueExpr =>
      cc.error(lhs, s"Invalid expression on left hand side of '$op='")
      StmtError() withLoc tree.loc

    case StmtPost(lhs, op) if !lhs.isLValueExpr =>
      cc.error(lhs, s"Invalid expression on left hand side of '$op'")
      StmtError() withLoc tree.loc

    case ExprCat(List(_)) =>
      cc.warning(tree, "Single expression concatenation")
      tree

    case _: StmtBreak if loopLevel == 0 =>
      cc.error(tree, "Break statements are only allowed inside looping statements")
      StmtError() withLoc tree.loc

    case _: StmtContinue if loopLevel == 0 =>
      cc.error(tree, "Continue statements are only allowed inside looping statements")
      StmtError() withLoc tree.loc

    case _: StmtLoop | _: StmtWhile | _: StmtFor | _: StmtDo =>
      tree tap { _ =>
        loopLevel -= 1
      }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(variantStack.isEmpty)
    assert(singletonStack.isEmpty)
    assert(loopLevel == 0)
  }
}

object Checker extends RootTransformerPass {
  val name = "checker"
  def create(implicit cc: CompilerContext) = new Checker
}
