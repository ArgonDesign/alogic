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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
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

  private def flattenThickets(tree: Tree): List[Tree] = tree match {
    case Thicket(trees) => trees
    case tree           => List(tree)
  }

  // Compute the generated tree from generate
  private def generate(gen: Gen, dispatcher: Generatable): List[Tree] = {
    def generateFor(bindings: Bindings,
                    terminate: Bindings => Option[Boolean],
                    loc: Loc,
                    body: List[Tree],
                    step: Stmt): List[Tree] = {
      val buf = new ListBuffer[Tree]

      @tailrec
      def loop(bindings: Bindings): Unit = terminate(bindings).value match {
        case None => cc.error(loc, "Condition of 'gen for' is not a compile time constant")
        case Some(false) =>
          val subGenerate = new Generate(bindings, Some(dispatcher))
          buf appendAll { body map subGenerate flatMap flattenThickets }
          loop(StaticEvaluation(step, bindings)._2)
        case _ => ()
      }

      loop(bindings)

      buf.toList
    }

    lazy val subGenerate = new Generate(bindings, Some(dispatcher))

    gen match {
      case GenIf(cond, thenItems, elseItems) =>
        typeCheck(cond) {
          (cond given bindings).value match {
            case None =>
              cc.error(cond, "Condition of 'gen if' is not a compile time constant")
              Nil
            case Some(v) if v != 0 => thenItems map subGenerate flatMap flattenThickets
            case _                 => elseItems map subGenerate flatMap flattenThickets
          }
        }

      case tree @ GenFor(inits, Some(cond), steps, body) =>
        typeCheck(inits ::: cond :: steps: _*) {
          if (cond.value.isDefined) {
            cc.error(cond, "'gen for' condition does not depend on loop variable")
            Nil
          } else {
            val initBindings = bindings ++ {
              inits map {
                case StmtDecl(Decl(symbol, Some(init))) => symbol -> init
                case _                                  => unreachable
              }
            }
            val StepStmt = TypeAssigner(StmtBlock(steps) withLoc tree.loc)
            val history = mutable.Set[Bindings]()
            val terminate = { bindings: Bindings =>
              val b = bindings mapValues { _.simplify }
              if (history contains b) {
                cc.error(cond, "'gen for' does not terminate")
                Some(true)
              } else {
                history add b
                (cond given bindings).value map { _ == 0 }
              }
            }
            generateFor(initBindings, terminate, cond.loc, body, StepStmt)
          }
        }

      case tree @ GenRange(decl @ Decl(symbol, None), op, end, body) =>
        // Build a spoof condition node for type checking only
        val cond = {
          val expr = (ExprRef(symbol) withLoc symbol.loc) < end
          expr.copy() withLoc decl.loc.copy(start = symbol.loc.start)
        }
        typeCheck(decl, end, cond) {
          // Some(Maximum inclusive value representable by symbol.kind) or None if infinite
          val maxValueOpt = if (symbol.kind.underlying.isNum) {
            None
          } else if (symbol.kind.isSigned) {
            Some(BigInt.mask(symbol.kind.width - 1))
          } else {
            Some(BigInt.mask(symbol.kind.width))
          }

          (end given bindings).value map { value =>
            if (op == "<") value - 1 else value // Inclusive end value
          } match {
            case None =>
              cc.error(end, "End value of range 'gen for' is not a compile time constant")
              Nil
            case Some(endValue) =>
              val lastValue = maxValueOpt map { _ min endValue } getOrElse endValue
              if (endValue > lastValue) {
                val v = end.value.get
                val t = symbol.kind.underlying.toSource
                cc.warning(
                  decl,
                  s"End value $v is out of range for variable ${symbol.name} with type '$t',",
                  s"will iterate only up to ${symbol.name} == ${maxValueOpt.get}"
                )
              }
              val init = if (symbol.kind.underlying.isNum) {
                ExprNum(symbol.kind.isSigned, 0) regularize symbol.loc
              } else {
                ExprInt(symbol.kind.isSigned, symbol.kind.width, 0) regularize symbol.loc
              }
              val iter = (BigInt(0) to lastValue).toIterator
              val terminate = { _: Bindings =>
                Some(if (iter.hasNext) { iter.next(); false } else true)
              }
              val stepStmt = StmtPost(ExprRef(symbol), "++") regularize decl.loc
              generateFor(bindings + (symbol -> init), terminate, Loc.synthetic, body, stepStmt)
          }
        }

      case GenFor(_, None, _, _)               => unreachable
      case GenRange(Decl(_, Some(_)), _, _, _) => unreachable
      case GenRange(_: DeclIdent, _, _, _)     => unreachable
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
        generated = Some(Nil) // To prevent further descent

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
