////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Check and strip static assertions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object CheckStaticAssertions {

  def apply(
      input: Either[Desc, (Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Option[Either[Desc, (Decl, Defn)]] = {

    var hadError = false

    def error(tree: Tree, msg: String): Unit = {
      hadError = true
      cc.error(tree, msg)
    }

    val transform = new StatelessTreeTransformer {

      override val typed = false

      override protected def skip(tree: Tree): Boolean = tree match {
        case _: Gen                   => true // Only check outside Gen
        case AssertionStatic(cond, _) =>
          // Don't check if the condition is unresolved
          cond exists {
            case _: Sym          => true
            case ExprSym(symbol) => !symbol.isSpecialized
          }
        case _: Expr => true
        case _       => false
      }

      override def transform(tree: Tree): Tree = tree match {
        case AssertionStatic(cond, msgOpt) =>
          if (cc.typeCheck(tree)) {
            cond.value match {
              case Some(v) if v != 0 => // OK
              case Some(_) =>
                val suffix = msgOpt map {
                  ": " + _
                } getOrElse ""
                error(cond, s"Static assertion failure$suffix")
              case None =>
                error(cond, "Cannot evaluate condition of static assertion at compilation time")
            }
          }
          Stump

        //
        case _ => tree
      }
    }

    val result = input.fold(
      desc => Left(desc rewrite transform),
      {
        case (decl, defn) => Right((decl, defn rewrite transform))
      }
    )

    Option.unless(hadError)(result)
  }

}
