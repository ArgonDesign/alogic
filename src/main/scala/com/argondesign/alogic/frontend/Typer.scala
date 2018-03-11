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
// The Typer:
// - Type checks the tree
// - Assigns types to all nodes
// - Infers widths of unsized constants
// - Folds expressions with unsized constants where necessary for width inference
// - Remove TypeDefinition nodes
// - Replace the Root node with the root Entity nodes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.ConstantFold
import com.argondesign.alogic.util.FollowedBy

final class Typer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  val constantFold = new ConstantFold

  val reducingBinaryOps = Array(">", ">=", "<", "<=", "==", "!=", "&&", "||")

  override def transform(tree: Tree): Tree = {
    require(!tree.hasTpe)

    val result: Tree = tree match {
      ////////////////////////////////////////////////////////////////////////////
      // Remove root node
      ////////////////////////////////////////////////////////////////////////////

      case node: Root => node.entity

      ////////////////////////////////////////////////////////////////////////////
      // Fold expressions with only unsized literals
      ////////////////////////////////////////////////////////////////////////////

      case ExprUnary(_, _: ExprNum)              => tree rewrite constantFold
      case ExprBinary(_: ExprNum, _, _: ExprNum) => tree rewrite constantFold

      ////////////////////////////////////////////////////////////////////////////
      // Infer width of of unsized literals
      ////////////////////////////////////////////////////////////////////////////

      case ExprBinary(lhs @ ExprNum(signed, value), op, rhs) => {
        require(rhs.hasTpe)

        // Type check and figure out width of rhs
        val widthOpt = rhs.tpe match {
          case kind if kind.isPacked => Some(kind.width) flatMap { _.value } map { _.toInt }
          case other => {
            cc.error(
              tree,
              s"Operator '+' expects packed type on the right hand side, but got  type '${other}'")
            None
          }
        }

        // Convert lhs
        widthOpt map { width =>
          val newLhs = ExprInt(signed, width, value) withLoc lhs.loc
          TypeAssigner(newLhs)
          ExprBinary(newLhs, op, rhs)
        } getOrElse {
          cc.error(rhs, s"Cannot infer width of right hand operand of operator '${op}'")
          ExprError()
        } withLoc lhs.loc
      }

      case ExprBinary(lhs, op, rhs @ ExprNum(signed, value)) => {
        require(lhs.hasTpe)

        // Type check and figure out width of lhs
        val widthOpt = lhs.tpe match {
          case kind if kind.isPacked => Some(kind.width) flatMap { _.value } map { _.toInt }
          case other => {
            cc.error(
              tree,
              s"Operator '+' expects packed type on the left hand side, but got  type '${other}'")
            None
          }
        }

        // Convert rhs
        widthOpt map { width =>
          val newRhs = ExprInt(signed, width, value) withLoc rhs.loc
          TypeAssigner(newRhs)
          ExprBinary(lhs, op, newRhs)
        } getOrElse {
          cc.error(rhs, s"Cannot infer width of left hand operand of operator '${op}'")
          ExprError()
        } withLoc lhs.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      //
      ////////////////////////////////////////////////////////////////////////////

      // Non-reducing unary ops
      case ExprUnary(op, expr) if op == "+" || op == "-" || op == "~" => {
        // TODO: Type check
        tree
      }

      // Reducing unary ops
      case ExprUnary(_, _) => {
        // TODO: Type check
        // TODO: Warn when reducing 1-bit quantity? Not good when it's parameterised
        tree
      }

      // Reducing binary ops
      case ExprBinary(lhs, op, rhs) if reducingBinaryOps contains op => {
        // TODO: Type check
        // TODO: Width warning, promotion
        tree
      }

      // Non-Reducing binary ops
      case ExprBinary(lhs, _, rhs) => {
        // TODO: Type check
        // TODO: Width warning, promotion, sizing of shift, etc, etc
        tree
      }

      case _ => tree
    }

    // Assign type of result if relevant node
    result match {
      case _: TypeDefinition => result // Will be removed
      case _: Entity => {
        tree match {
          case _: Root => result // Already typed the root entity
          case _       => TypeAssigner(result)
        }
      }
      case _ => TypeAssigner(result)
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    val isEntity = tree match {
      case _: Entity => true
      case _         => false
    }

    if (isEntity) {
      tree visit {
        case _: Type => /* Don't recurse into types */
        case node: ExprNum => {
          cc.ice(
            node,
            "Typer should have removed all unsized integer literals, but the following remains",
            node.toString
          )
        }
        case node: Tree if !node.hasTpe => {
          cc.ice(
            node,
            "Typer should have assigned type for all node, but the following is untyped",
            node.toString
          )
        }
        case node: Tree if node.tpe == TypeUnknown => {
          cc.ice(node, s"Could not compute type of ${node}")
        }
      }
    }

    tree visit {
      case node: TypeDefinition => {
        cc.ice(node, s"Typer should have removed type definitions, but '${node}' remains")
      }
      case node: Root => {
        cc.ice(node, s"Typer should have removed the Root node")
      }
    }
  }

}
