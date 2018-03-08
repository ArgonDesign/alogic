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
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions
import com.argondesign.alogic.util.FollowedBy

final class Typer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy { namer =>

  private implicit def boolean2BigInt(bool: Boolean) = if (bool) BigInt(1) else BigInt(0)

  override def enter(tree: Tree): Unit = tree match {

    case _ => ()
  }

  val reducingBinaryOps = Array(">", ">=", "<", "<=", "==", "!=", "&&", "||")

  override def transform(tree: Tree): Tree = tree match {
    case node: Root => node.entity

    ////////////////////////////////////////////////////////////////////////////
    // Type references based on symbol
    ////////////////////////////////////////////////////////////////////////////

    case ExprRef(Sym(symbol)) if symbol != ErrorSymbol =>
      {
        val kind = symbol.denot.kind match {
          case TypeParam(kind)      => kind
          case TypeConst(kind)      => kind
          case TypePipeline(kind)   => kind
          case TypeRef(Sym(symbol)) => symbol.denot.kind
          case other                => other
        }
        tree withTpe kind
      }

    ////////////////////////////////////////////////////////////////////////////
    // Propagate error expressions
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(_, expr: ExprError)      => expr
    case ExprBinary(expr: ExprError, _, _)  => expr
    case ExprBinary(_, _, expr: ExprError)  => expr
    case ExprTernary(expr: ExprError, _, _) => expr
    case ExprTernary(_, expr: ExprError, _) => expr
    case ExprTernary(_, _, expr: ExprError) => expr

    ////////////////////////////////////////////////////////////////////////////
    // Fold expressions with only unsized constants
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, expr @ ExprNum(signed, value)) => op match {
      // Valid cases
      case "+"           => expr
      case "-" if signed => ExprNum(true, -value) withLoc tree.loc
      case "!"           => ExprNum(signed, value == 0) withLoc tree.loc
      // Invalid cases
      case "-" if !signed => ExprError() withLoc tree.loc followedBy {
        cc.error(tree, "Unary operator '-' is not well defined for unsized unsigned values")
      }
      case op => ExprError() withLoc tree.loc followedBy {
        cc.error(tree, s"Unary operator '${op}' is not well defined for unsized values")
      }
    }

    case ExprBinary(ExprNum(ls, lv), op, ExprNum(rs, rv)) => {
      val negl = lv < 0
      val negr = rv < 0
      val nege = negl || negr
      val num = (ls, op, rs) match {
        case (true, "*", true)          => ExprNum(true, lv * rv)
        case (true, "/", true)          => ExprNum(true, lv / rv)
        case (true, "%", true)          => ExprNum(true, lv % rv)
        case (true, "+", true)          => ExprNum(true, lv + rv)
        case (true, "-", true)          => ExprNum(true, lv - rv)
        case (true, "&", true) if !nege => ExprNum(true, lv & rv)
        case (true, "^", true) if !nege => ExprNum(true, lv ^ rv)
        case (true, "|", true) if !nege => ExprNum(true, lv | rv)

        case (true, op @ ("&" | "^" | "|"), true) => ExprError() followedBy {
          cc.error(tree, s"Binary operator '${op}' is not well defined for negative unsized values")
        }
        case (true, "~^", true) => ExprError() followedBy {
          cc.error(tree, "Binary operator '~^' is not well defined between unsized values")
        }

        case (true, "<<", _)  => ???
        case (true, ">>", _)  => ???
        case (true, ">>>", _) => ???
        case (true, "<<<", _) => ???
        case (_, ">", _)      => ExprNum(false, lv > rv)
        case (_, "<", _)      => ExprNum(false, lv < rv)
        case (_, ">=", _)     => ExprNum(false, lv >= rv)
        case (_, "<=", _)     => ExprNum(false, lv <= rv)
        case (_, "==", _)     => ExprNum(false, lv == rv)
        case (_, "!=", _)     => ExprNum(false, lv != rv)
        case (true, "&&", _)  => ???
        case (true, "||", _)  => ???

        case _                => unreachable
      }
      num withLoc tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////

    // Non-reducing unary ops
    case ExprUnary(op, expr) if op == "+" || op == "-" || op == "~" => {
      // TODO: Type check
      tree withTpe expr.tpe
    }

    // Reducing unary ops
    case ExprUnary(_, _) => {
      // TODO: Type check
      // TODO: Warn when reducing 1-bit quantity? Not good when it's parameterised
      tree withTpe TypeUInt(1)
    }

    // Reducing binary ops
    case ExprBinary(lhs, op, rhs) if reducingBinaryOps contains op => {
      // TODO: Type check
      // TODO: Width warning, promotion
      tree withTpe TypeUInt(1)
    }

    // Non-Reducing binary ops
    case ExprBinary(lhs, _, rhs) => {
      // TODO: Type check
      // TODO: Width warning, promotion, sizing of shift, etc, etc
      val tpe = if (lhs.tpe.asInstanceOf[TypeInt].signed) rhs.tpe else lhs.tpe
      tree withTpe tpe
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    val isEntity = tree match {
      case _: Entity => true
      case _         => false
    }
    tree visit {
      case _: Type => /* Don't recurse into types */
      case node: ExprNum if isEntity => {
        cc.ice(node, s"Typer should have removed all unsized integer literals, but '${node}' remains")
      }
      case node: TypeDefinition => {
        cc.ice(node, s"Typer should have removed type definitions, but '${node}' remains")
      }
      case node: Root => {
        cc.ice(node, s"Typer should have removed the Root node")
      }
      //      case node: Tree if !node.hasTpe && isEntity => {
      //        cc.ice(node, s"Type shuld have assigned type for node ${node}")
      //      }
    }
  }

}
