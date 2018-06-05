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

import com.argondesign.alogic.ast.TreeCopier
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

final class Checker(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  override val typed: Boolean = false

  private[this] var entityLevel = 0

  override def enter(tree: Tree): Unit = tree match {
    case _: Entity => entityLevel += 1
    case _         =>
  }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity => {
      val variant = entity.variant match {
        case "fsm"      => "fsm"
        case "network"  => "network"
        case "verbatim" => "verbatim"
        case other      => cc.ice(entity, s"Unknown entity variant '$other'")
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
          cc.error(_, s"Multiple fence blocks specified in entity '${name}'")
        }
        Nil
      } else {
        fenceBlocks
      }

      val declarations = {
        val (goodDecls, badDecls) = variant match {
          case "fsm" => {
            entity.declarations.partition {
              case decl: DeclIdent => {
                decl.kind match {
                  case _: TypePipeline => false
                  case _               => true
                }
              }
              case _ => unreachable
            }
          }
          case "network" => {
            entity.declarations.partition {
              case decl: DeclIdent => {
                decl.kind match {
                  case _: TypeIn       => true
                  case _: TypeOut      => true
                  case _: TypeParam    => true
                  case _: TypeConst    => true
                  case _: TypePipeline => true
                  case _               => false
                }
              }
              case _ => unreachable
            }
          }
          case "verbatim" => {
            entity.declarations.partition {
              case decl: DeclIdent => {
                decl.kind match {
                  case _: TypeIn    => true
                  case _: TypeOut   => true
                  case _: TypeParam => true
                  case _: TypeConst => true
                  case _: TypeSram  => true
                  case _            => false
                }
              }
              case _ => unreachable
            }
          }
          case _ => unreachable
        }

        badDecls foreach {
          case decl: DeclIdent => {
            val hint = decl.kind match {
              case _: TypeArray    => "distributed memory"
              case _: TypePipeline => "pipeline variable"
              case _: TypeSram     => "SRAM"
              case _               => "variable"
            }
            err(List(decl), s"${hint} declarations")
          }
          case _ => unreachable
        }

        goodDecls
      }

      if (variant != "verbatim" &&
          entity.verbatim.nonEmpty &&
          instances.isEmpty &&
          connects.isEmpty &&
          functions.isEmpty &&
          fenceStmts.isEmpty &&
          entities.isEmpty) {
        cc.warning(
          entity.ref,
          s"Entity '${entity.ref.toSource}' contains only verbatim blocks, use a 'verbatim entity' instead")
      }

      TreeCopier(entity)(
        entity.ref,
        declarations,
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

    case decl @ DeclIdent(Ident(name), TypeOut(kind, fc, st), None) => {

      val newSt = (fc, st) match {
        case (FlowControlTypeNone, _: StorageTypeSlices) => {
          cc.error(tree,
                   s"Output port '${name}' without flow control specifier cannot use output slices")
          StorageTypeReg
        }
        case (FlowControlTypeValid, _: StorageTypeSlices) => {
          cc.error(
            tree,
            s"Output port '${name}' with 'sync' flow control specifier cannot use output slices")
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

    case decl @ DeclIdent(_, kind, Some(init)) => {
      val hintOpt = kind match {
        case _: TypeIn => Some("Input port declarations")
        case TypeOut(_, FlowControlTypeNone, StorageTypeWire) => {
          Some("Output port with 'wire' storage specifier")
        }
        case TypeOut(_, FlowControlTypeValid, _) => {
          Some("Output port declarations with 'sync' flow control")
        }
        case TypeOut(_, FlowControlTypeReady, _) => {
          Some("Output port declarations with 'sync ready' flow control")
        }
        case TypeOut(_, FlowControlTypeAccept, _) => {
          Some("Output port declarations with 'sync accept' flow control")
        }
        case _: TypePipeline => Some("Pipeline variable declarations")
        case _: TypeArray    => Some("Array declarations")
        case _: TypeSram     => Some("SRAM declarations")
        case _               => None
      }

      hintOpt map { hint =>
        cc.error(init, s"${hint} cannot have an initializer")
        decl.copy(init = None) withLoc decl.loc
      } getOrElse decl
    }

    case decl @ DeclIdent(_, kind, None) => {
      val hintOpt = kind match {
        case _: TypeParam => Some("Parameter")
        case _: TypeConst => Some("Constant")
        case _            => None
      }

      hintOpt map { hint =>
        cc.error(decl, s"${hint} declarations must have an initializer")
        val num = ExprNum(true, 0) withLoc decl.loc
        decl.copy(init = Some(num)) withLoc decl.loc
      } getOrElse decl
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

    case StmtDecl(decl: DeclIdent) => {
      def msg(kind: Type) = kind match {
        case _: TypeIn       => Some("Input ports must be declared in the enclosing entity")
        case _: TypeOut      => Some("Output ports must be declared in the enclosing entity")
        case _: TypeParam    => Some("Parameters must be declared in the enclosing entity")
        case _: TypeConst    => Some("Constants must be declared in the enclosing entity")
        case _: TypeArray    => Some("Arrays must be declared in the enclosing entity")
        case _: TypePipeline => Some("Pipeline variables must be declared in the outer entity")
        case _: TypeSram     => Some("SRAMs must be declared in the enclosing entity")
        case _               => None
      }

      msg(decl.kind) map { hint =>
        cc.error(tree, "Only variables can be declared in declaration statements", hint)
        StmtError() withLoc tree.loc
      } getOrElse tree
    }

    case StmtCase(expr, cases, default) if default.length > 1 => {
      default foreach {
        cc.error(_, "Multiple 'default' clauses specified in case statement")
      }
      StmtCase(expr, cases, Nil) withLoc tree.loc
    }

    case StmtAssign(lhs, _) => {
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, "Invalid expression on left hand side of '='")
        StmtError() withLoc tree.loc
      }
    }

    case StmtUpdate(lhs, op, _) => {
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, s"Invalid expression on left hand side of '${op}='")
        StmtError() withLoc tree.loc
      }
    }

    case StmtPost(lhs, op) => {
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, s"Invalid expression on left hand side of '${op}'")
        StmtError() withLoc tree.loc
      }
    }

    case connect @ Connect(lhs, rhss) => {
      val hint = "Only identifiers, optionally followed by a single field selector are allowed"

      val newLhs = if (lhs.isPortRefExpr) {
        lhs
      } else {
        cc.error(lhs, s"Invalid port reference on left hand side of '->'", hint)
        ExprError() withLoc lhs.loc
      }

      val (goodRhss, badRhss) = rhss partition { _.isPortRefExpr }

      badRhss foreach {
        cc.error(_, s"Invalid port reference on right hand side of '->'", hint)
      }

      val newRhss = if (badRhss.isEmpty) rhss else goodRhss

      TreeCopier(connect)(newLhs, newRhss)
    }

    case ExprCat(List(_)) => {
      cc.warning(tree, "Single expression concatenation")
      tree
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(entityLevel == 0)
  }

}

object Checker extends TreeTransformerPass {
  val name = "checker"
  def create(implicit cc: CompilerContext) = new Checker
}
