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

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._

final class InferImplications(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: EntityLowered                           => false
    case Connect(_, List(ExprRef(rhs: TermSymbol))) => rhs.kind.width != 1
    case _                                          => true
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      case Connect(source, List(ExprRef(dst: TermSymbol))) => {
        source match {
          case ExprRef(src: TermSymbol) => {
            src.attr.implications.append((true, true, dst))
            src.attr.implications.append((false, false, dst))
            dst.attr.implications.append((true, true, src))
            dst.attr.implications.append((false, false, src))
          }
          case ExprUnary("~" | "!", ExprRef(src: TermSymbol)) => {
            src.attr.implications.append((false, true, dst))
            src.attr.implications.append((true, false, dst))
            dst.attr.implications.append((false, true, src))
            dst.attr.implications.append((true, false, src))
          }
          case _ =>
        }
      }

      case entity: EntityLowered => {
        // Transitively propagate all implication relations within the entity
        def loop(): Unit = {
          val found = for {
            Decl(aSymbol, _) <- entity.declarations
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
      }

      case _ =>
    }

    tree
  }

}

object InferImplications extends TreeTransformerPass {
  val name = "infer-implications"
  def create(implicit cc: CompilerContext) = new InferImplications
}
