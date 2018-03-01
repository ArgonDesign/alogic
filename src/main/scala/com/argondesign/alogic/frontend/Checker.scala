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

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.ast.Trees.ExprAtCall
import com.argondesign.alogic.ast.TreeCopier

final class Checker(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  private[this] var entityLevel = 0

  override def enter(tree: Tree): Unit = tree match {
    case _: Entity => entityLevel += 1
    case _         =>
  }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity => {
      val variant = entity.variant match {
        case "fsm"     => "fsm"
        case "network" => "network"
        case "verilog" => "verilog"
        case other     => cc.ice(entity, s"Unknown entity variant '$other'")
      }

      def err(nodes: List[Tree], content: String) = {
        nodes foreach { cc.error(_, s"'${variant}' entity cannot contain ${content}") }
        Nil
      }

      val instances = variant match {
        case "network" => entity.instances
        case _         => err(entity.instances, "instantiations")
      }

      val connects = variant match {
        case "network" => entity.connects
        case _         => err(entity.connects, "connections")
      }

      val functions = variant match {
        case "fsm" => entity.functions
        case _     => err(entity.functions, "function definitions")
      }

      val fenceBlocks = variant match {
        case "fsm" => entity.fenceStmts
        case _     => err(entity.fenceStmts, "fence blocks")
      }

      val entities = variant match {
        case "network" => entity.entities
        case _         => err(entity.entities, "nested entities")
      }

      val fenceStmts = if (fenceBlocks.length > 1) {
        val Ident(name) = entity.ref
        fenceBlocks foreach {
          cc.error(_, s"More than 1 fence blocks specified in entity '${name}'")
        }
        Nil
      } else {
        fenceBlocks
      }

      TreeCopier(entity)(
        entity.ref,
        entity.declarations,
        instances,
        connects,
        functions,
        entity.states,
        fenceStmts,
        entities
      )
    } followedBy {
      entityLevel -= 1
    }

    case decl @ Decl(_, TypeOut(kind, fc, st), _) => {
      lazy val Ident(name) = decl.ref

      val newSt = (fc, st) match {
        case (FlowControlTypeNone, _: StorageTypeSlices) => {
          cc.error(tree, s"Output port '${name}' without flow control specifier cannot use output slices")
          StorageTypeReg
        }
        case (FlowControlTypeValid, _: StorageTypeSlices) => {
          cc.error(tree, s"Output port '${name}' with 'sync' flow control specifier cannot use output slices")
          StorageTypeReg
        }
        case _ => st
      }

      if (newSt eq st) {
        tree
      } else {
        decl.copy(kind = TypeOut(kind, fc, newSt)) withLoc tree.loc
      }
    }

    case ExprAtCall("bits", arg :: _) => {
      def offender(expr: Expr): Option[Expr] = {
        expr match {
          case _: ExprRef          => None
          case ExprSelect(from, _) => offender(from)
          case node: Expr          => Some(node)
        }
      }
      offender(arg) map { expr =>
        cc.error(
          expr,
          "Invalid expression passed to '@bits'",
          "Only identifiers, optionally followed by field lookups are allowed"
        )
        ExprError() withLoc tree.loc
      } getOrElse tree
    }

    case StmtRead() => {
      if (entityLevel <= 1) {
        cc.error(tree, "Read statements are only allowed inside nested entities")
        StmtError() withLoc tree.loc
      } else {
        tree
      }
    }

    case StmtWrite() => {
      if (entityLevel <= 1) {
        cc.error(tree, "Write statements are only allowed inside nested entities")
        StmtError() withLoc tree.loc
      } else {
        tree
      }
    }

    case StmtDecl(decl) => {
      def msg(kind: Type) = kind match {
        case _: TypeIn       => Some("Input ports must be declared in the entity")
        case _: TypeOut      => Some("Output ports must be declared in the entity")
        case _: TypeParam    => Some("Parameters must be declared in the entity")
        case _: TypeConst    => Some("Constants must be declared in the entity")
        case _: TypeArray    => Some("Arrays must be declared in the entity")
        case _: TypePipeline => Some("Pipeline variables must be declared in the outer entity")
        case _               => None
      }

      msg(decl.kind) map { line =>
        cc.error(tree, "Only variables can be declared in declaration statements", line)
        StmtError() withLoc tree.loc
      } getOrElse tree
    }

    case StmtCase(expr, cases, default) if default.length > 1 => {
      default foreach {
        cc.error(_, "Multiple 'default' clauses specified in case statement")
      }
      StmtCase(expr, cases, Nil) withLoc tree.loc
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(entityLevel == 0)
  }

}
