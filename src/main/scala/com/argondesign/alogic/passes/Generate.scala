////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Process 'gen' statements
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

private trait Generatable {
  def isExpectedType(tree: Tree): Boolean
}

private object generatableStmt extends Generatable {
  def isExpectedType(tree: Tree): Boolean = tree.isInstanceOf[Stmt]
}

private final class Generate(
    bindings: Bindings,
    dispatcher: Option[Generatable] // Some, if already inside a 'gen' else None
)(
    implicit cc: CompilerContext,
    typer: Typer
) extends TreeTransformer {

  override val typed: Boolean = false

  // Type check trees, yield Nil if type error, otherwise yield result
  private def typeCheck(trees: Tree*)(result: => List[Tree]): List[Tree] = {
    if (trees map typer exists { _.tpe.isError }) Nil else result
  }

  // Compute the generated tree from generate
  private def generate(tree: Tree, dispatcher: Generatable): List[Tree] = {
    def interpretGenFor(bindings: Bindings,
                        cond: Expr,
                        body: List[Tree],
                        step: Stmt): List[Tree] = {
      val buf = new ListBuffer[Tree]

      @tailrec
      def loop(bindings: Bindings): Unit = (cond given bindings).value match {
        case None              => cc.error(cond, "Condition of 'gen for' is not a compile time constant")
        case Some(v) if v != 0 =>
          // TODO: ensure no infinite loop
          val subGenerate = new Generate(bindings, Some(dispatcher))
          buf appendAll { body map subGenerate }
          loop(StaticEvaluation(step, bindings)._2)
        case _ => ()
      }

      loop(bindings)

      buf.toList
    }

    lazy val subGenerate = new Generate(bindings, Some(dispatcher))

    tree match {
      case GenIf(cond, thenItems, elseItems) =>
        typeCheck(cond) {
          (cond given bindings).value match {
            case None =>
              cc.error(cond, "Condition of 'gen if' is not a compile time constant")
              Nil
            case Some(v) if v != 0 => thenItems map subGenerate
            case _                 => elseItems map subGenerate
          }
        }

      case tree @ GenFor(inits, Some(cond), step, body) =>
        typeCheck(inits ::: cond :: step: _*) {
          val initBindings = bindings ++ {
            inits map {
              case StmtDecl(Decl(symbol, Some(init))) => symbol -> init
              case _                                  => unreachable
            }
          }
          val stepBlock = TypeAssigner(StmtBlock(step) withLoc tree.loc)
          interpretGenFor(initBindings, cond, body, stepBlock)
        }

      case _: Gen => ???

      case _ => unreachable
    }
  }

  // Result of the outermost encountered Gen constcuct
  private var generated: Option[List[Tree]] = None

  // Tree level
  private var level: Int = 0

  // Only go one Gen deep
  override def skip(tree: Tree): Boolean = generated.nonEmpty

  override def enter(tree: Tree): Unit = {
    assert(!tree.isInstanceOf[Gen] || generated.isEmpty)
    tree match {
      // Root Gen that must produce statement
      case StmtGen(gen) =>
        generated = Some(generate(gen, generatableStmt))

      // Gen nested inside outer Gen node, must produce whatever it's nested inside
      case gen: Gen =>
        assert(dispatcher.nonEmpty)
        generated = Some(generate(gen, dispatcher.get))

      // Keep going if not inside a Gen
      case _ if dispatcher.isEmpty => ()

      // If inside a Gen check this is the right kind of tree
      case _ if level == 0 && dispatcher.get.isExpectedType(tree) => ()

      // Otherwise die
      case _ if level == 0 =>
        cc.error(tree, "'gen' construct yields invalid syntax")
        generated = Some(Nil)

      case _ => ()
    }
    level += 1
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      // Replace outermost gen with the generate result
      case _: StmtGen => Thicket(generated.get) withLoc tree.loc

      // Replace nested gen with the generated result
      case _: Gen => Thicket(generated.get) withLoc tree.loc

      // Replace refs to bound symbols with the value
      case ExprRef(symbol: TermSymbol) => bindings.getOrElse(symbol, tree)

      case _ => tree
    }
  } followedBy {
    generated = None
    level -= 1
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(generated.isEmpty)
    assert(level == 0)
    tree visit {
      case node: Gen => cc.ice(node, s"Gen remains")
    }
  }
}

object Generate extends TreeTransformerPass {
  val name = "generate"
  def create(implicit cc: CompilerContext): TreeTransformer =
    new Generate(Bindings.empty, None)(cc, new Typer)
}
