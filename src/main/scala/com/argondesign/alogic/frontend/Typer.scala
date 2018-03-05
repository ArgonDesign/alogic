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

final class Typer(implicit cc: CompilerContext) extends TreeTransformer { namer =>

  override def enter(tree: Tree): Unit = tree match {

    case _ => ()
  }

  val reducingBinaryOps = Array(">", ">=", "<", "<=", "==", "!=", "&&", "||")

  override def transform(tree: Tree): Tree = tree match {
    case node: Root => node.entity

    case ExprRef(Sym(symbol)) if symbol != ErrorSymbol => {
      val kind = symbol.denot.kind match {
        case TypeParam(kind)      => kind
        case TypeConst(kind)      => kind
        case TypePipeline(kind)   => kind
        case TypeRef(Sym(symbol)) => symbol.denot.kind
        case other                => other
      }
      tree withTpe kind
    }

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
    case ExprBinary(_, op, _) if reducingBinaryOps contains op => {
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
    tree visit {
      case _: Type => /* Don't recurse into types */
      case node @ ExprNum(_, None, _) => {
        cc.ice(node, s"Typer should have removed all unsized integer literals, but '${node}' remains")
      }
      case node: TypeDefinition => {
        cc.ice(node, s"Typer should have removed type definitions, but '${node}' remains")
      }
      case node: Root => {
        cc.ice(node, s"Typer should have removed the Root node")
      }
    }
  }

}
