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
// A pure analysis pass that infers signal implication relationships based
// on Connect nodes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol

import scala.annotation.tailrec

final class InferImplications(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: DefnEntity              => false
    case EntAssign(ExprSym(lhs), _) => lhs.kind.width != 1
    case _                          => true
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      case EntAssign(ExprSym(dst), rhs) =>
        rhs match {
          case ExprSym(src) =>
            src.attr.implications.append((true, true, dst))
            src.attr.implications.append((false, false, dst))
            dst.attr.implications.append((true, true, src))
            dst.attr.implications.append((false, false, src))
          case ExprUnary("~" | "!", ExprSym(src)) =>
            src.attr.implications.append((false, true, dst))
            src.attr.implications.append((true, false, dst))
            dst.attr.implications.append((false, true, src))
            dst.attr.implications.append((true, false, src))
          case _ =>
        }

      case defn: DefnEntity =>
        // Transitively propagate all implication relations within the entity
        @tailrec
        def loop(): Unit = {
          val found = for {
            Defn(aSymbol) <- defn.defns
            (da, db, bSymbol) <- aSymbol.attr.implications.enumerate
            (tb, tc, cSymbol) <- bSymbol.attr.implications.enumerate
            if db == tb && cSymbol != aSymbol
            transitive = (da, tc, cSymbol)
            if !(aSymbol.attr.implications.enumerate contains transitive)
          } yield {
            aSymbol.attr.implications append transitive
          }
          if (found.nonEmpty) loop()
        }
        loop()

      case _ =>
    }

    tree
  }

}

object InferImplications extends EntityTransformerPass(declFirst = true) {
  val name = "infer-implications"
  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = cc.inferImplications
}
