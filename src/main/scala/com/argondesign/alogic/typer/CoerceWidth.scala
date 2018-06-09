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
// Retype is used for inference, where a Tree is re-written if possible to
// yield the expected type provided
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeError
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

final class CoerceWidth(width: Int)(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  override val typed = false

  lazy val minSig = -(BigInt(1) << (width - 1))
  lazy val maxSig = (BigInt(1) << (width - 1)) - 1
  lazy val minUns = BigInt(0)
  lazy val maxUns = (BigInt(1) << width) - 1

  private[this] def coerceNum(num: ExprNum) = {
    val ExprNum(signed, value) = num
    val err = if (signed) {
      if (value > maxSig) {
        cc.error(num, s"Value ${value} is too large to fit in ${width} signed bits")
        true
      } else if (value < minSig) {
        cc.error(num, s"Value ${value} is too small to fit in ${width} signed bits")
        true
      } else {
        false
      }
    } else {
      if (value > maxUns) {
        cc.error(num, s"Value ${value} is too large to fit in ${width} unsigned bits")
        true
      } else if (value < minUns) {
        unreachable
        true
      } else {
        false
      }
    }

    if (err) {
      ExprError() withLoc num.loc
    } else {
      ExprInt(signed, width, value) withLoc num.loc
    }
  }

  // We keep a stack of Booleans that indicate whether the current node should be coerced
  private[this] var stack: List[Boolean] = List(true)
  private[this] def push(coerce: Boolean) = { stack = coerce :: stack }
  private[this] def pop() = { stack = stack.tail }
  private[this] def coerceCurrent: Boolean = stack.head

  override def enter(tree: Tree): Unit = tree match {
    case _: ExprNum => ()

    case ExprUnary("!" | "&" | "|" | "^", _) => {
      push(false) // Do not coerce the operand
    }

    case _: ExprUnary => {
      push(coerceCurrent) // Coerce the operand if needed
    }

    case ExprBinary(_, ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||", _) => {
      push(false) // Do not coerce the rhs
      push(false) // Do not coerce the lhs
    }

    case ExprBinary(_, "<<" | ">>" | "<<<" | ">>>", _) => {
      val coerce = coerceCurrent
      push(false) // Do not coerce the rhs
      push(coerce) // Coerce lhs if needed
    }

    case _: ExprBinary => {
      push(coerceCurrent) // Coerce rhs if needed
      push(coerceCurrent) // Coerce lhs if needed
    }

    case _: ExprTernary => {
      push(coerceCurrent) // Coerce elseExpr if needed
      push(coerceCurrent) // Coerce thenExpr if needed
      push(false) // Do not coerce condition
    }

    case _ if coerceCurrent => cc.ice(tree, s"Don't know how to coerce '${tree}'")

    case _ => {
      assert(!coerceCurrent)
      // Do not coerce any children
      tree.children foreach {
        case _: Tree => push(false)
        case _       => ()
      }
    }
  }

  private def hasError(tree: Tree) = {
    tree.children exists {
      case child: Tree => child.tpe == TypeError
      case _: Type     => false
      case _           => unreachable
    }
  }

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {
      case _: ExprError                  => tree
      case expr if hasError(expr)        => ExprError() withLoc tree.loc
      case num: ExprNum if coerceCurrent => coerceNum(num)
      case tree                          => tree
    }

    if (!result.hasTpe) {
      TypeAssigner(result)
    }

    result
  } followedBy {
    pop()
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(stack.isEmpty)
  }

}

object CoerceWidth {

  def apply(expr: Expr, width: Int)(implicit cc: CompilerContext): Expr = {
    (expr rewrite new CoerceWidth(width)).asInstanceOf[Expr]
  }

  def apply(expr: Expr, width: BigInt)(implicit cc: CompilerContext): Expr = {
    apply(expr, width.toInt)
  }

}
