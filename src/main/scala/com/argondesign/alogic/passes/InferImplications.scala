////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// A pure analysis pass that infers signal implication relationships based
// on Connect nodes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec

object InferImplicationsTransform extends StatelessTreeTransformer {

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    case _: DefnEntity                                     => None
    case EntAssign(ExprSym(lhs), _) if lhs.kind.width == 1 => None
    case _                                                 => Some(tree)
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

      case _ => unreachable // Due to skip
    }

    tree
  }

}

object InferImplications extends EntityTransformerPass(declFirst = true, parallel = true) {
  val name = "infer-implications"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    InferImplicationsTransform
}
