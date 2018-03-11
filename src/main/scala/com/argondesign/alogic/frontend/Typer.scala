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
// - Assigns types to all nodes using the TypeAssigner
// - Infers widths of unsized constants
// - Folds expressions with unsized constants where necessary for width inference
// - Remove TypeDefinition nodes
// - Replace the Root node with the root Entity node
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

    // TODO: warn for field hiding by extension types

    val result: Tree = tree match {
      ////////////////////////////////////////////////////////////////////////////
      // Remove root node
      ////////////////////////////////////////////////////////////////////////////

      case node: Root => node.entity

      ////////////////////////////////////////////////////////////////////////////
      // Propagate errors
      ////////////////////////////////////////////////////////////////////////////

      ////////////////////////////////////////////////////////////////////////////
      // Infer width of of unsized literals
      ////////////////////////////////////////////////////////////////////////////

      case ExprBinary(lhs @ ExprNum(signed, value), op, rhs) => {
        require(rhs.hasTpe)

        // Type check and get expected type
        val kindOpt = rhs.tpe match {
          case kind if kind.isPacked => Some(kind)
          case other => {
            cc.error(tree, s"'${op}' expects packed value on the right hand side")
            None
          }
        }

        // Figure out width of rhs
        val widthOpt = kindOpt flatMap { _.width.value } map { _.toInt }

        if (kindOpt.isDefined && widthOpt.isEmpty) {
          cc.error(rhs, s"Cannot infer width of left hand operand of '${op}'")
        }

        // Convert lhs
        widthOpt map { width =>
          // TODO: Check value fits width ...
          val newLhs = ExprInt(signed, width, value) withLoc lhs.loc
          TypeAssigner(newLhs)
          ExprBinary(newLhs, op, rhs)
        } getOrElse {
          ExprError()
        } withLoc lhs.loc
      }

      case ExprBinary(lhs, op, rhs @ ExprNum(signed, value)) => {
        require(lhs.hasTpe)

        // Type check and get expected type
        val kindOpt = lhs.tpe match {
          case kind if kind.isPacked => Some(kind)
          case other => {
            cc.error(tree, s"'${op}' expects packed value on the left hand side")
            None
          }
        }

        // Figure out width of rhs
        val widthOpt = kindOpt flatMap { _.width.value } map { _.toInt }

        if (kindOpt.isDefined && widthOpt.isEmpty) {
          cc.error(rhs, s"Cannot infer width of right hand operand of '${op}'")
        }

        // Convert rhs
        widthOpt map { width =>
          // TODO: Check value fits width ...
          val newRhs = ExprInt(signed, width, value) withLoc rhs.loc
          TypeAssigner(newRhs)
          ExprBinary(lhs, op, newRhs)
        } getOrElse {
          ExprError()
        } withLoc lhs.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check expressions
      ////////////////////////////////////////////////////////////////////////////

      case ExprSelect(expr, selector) => {
        val field = expr.tpe match {
          case TypeType(kind: CompoundType) => kind(selector)
          case kind: CompoundType           => kind(selector)
          case _                            => None
        }

        if (field.isDefined) {
          tree
        } else {
          cc.error(tree, s"No field named '${selector}' in '${expr}'")
          ExprError() withLoc tree.loc
        }
      }

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
      case ExprBinary(lhs, op, rhs) => {
        require(lhs.hasTpe)
        require(rhs.hasTpe)

        // Check both sides are packed types
        if (!lhs.tpe.isPacked || !rhs.tpe.isPacked) {
          if (!lhs.tpe.isPacked) {
            cc.error(tree, s"'${op}' expects packed value on the left hand side")
          }
          if (!rhs.tpe.isPacked) {
            cc.error(tree, s"'${op}' expects packed value on the right hand side")
          }
          ExprError() withLoc tree.loc
        } else {
          // TODO: handle parametrized widths by solving 'lhsWidth != rhsWidth' SAT
          val lhsWidthOpt = lhs.tpe.width.value
          val rhsWidthOpt = rhs.tpe.width.value

          for {
            lhsWidth <- lhsWidthOpt
            rhsWidth <- rhsWidthOpt
          } {
            if (lhsWidth != rhsWidth) {
              cc.warning(
                tree,
                s"'${op}' expects both operands to have the same width, but",
                s"left  operand is ${lhsWidth} bits wide, and",
                s"right operand is ${rhsWidth} bits wide"
              )
            }

          }

          tree
        }

      }

      case _ => tree
    }

    // Assign type if have not been assigned by now
    if (result.hasTpe) result else TypeAssigner(result)
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