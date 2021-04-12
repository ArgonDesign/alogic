////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// A pass is anything that takes a list of trees and returns a new list
// of trees. While most of them are implemented as a TreeTransformer,
// other implementations are possible but discouraged. If a pass can
// reasonably be implemented as a TreeTransformer, it should be.
// Passes should implement a single transformation or analysis function.
// All passes should process all trees in parallel whenever possible.
// Splitting a pass into multiple smaller passes is recommended if this
// enables running the smaller passes in parallel across all trees.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol

import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

// The compiler pass interface
sealed trait Pass[-T, +R] { self =>

  //////////////////////////////////////////////////////////////////////////////
  // Abstract members
  //////////////////////////////////////////////////////////////////////////////

  // Return an iterable of the transformations to be applied by this pass.
  def passTransformations: Iterator[(Any, Int, CompilerContext) => Option[Any]]

  //////////////////////////////////////////////////////////////////////////////
  // Concrete members
  //////////////////////////////////////////////////////////////////////////////

  // Apply the pass to some input
  final def apply(input: T)(implicit cc: CompilerContext): Option[R] =
    passTransformations.foldLeft[(Option[Any], Int)]((Some(input), 0)) {
      case (pair @ (None, _), _) => pair
      case ((Some(input), n), f) => (f(input, n, cc), n + 1)
    } pipe {
      _._1.asInstanceOf[Option[R]]
    }

  // Combine with subsequent pass, with this Pass applied first
  final def andThen[U](next: Pass[R, U]): Pass[T, U] = new CombinedPass(this, next)
}

// Extend this for passes implementing a single transformation
trait SimplePass[T, R] extends Pass[T, R] { self =>
  //////////////////////////////////////////////////////////////////////////////
  // Abstract members
  //////////////////////////////////////////////////////////////////////////////

  // Name of pass for debugging
  val name: String
  // The implementation of the pass transformation
  protected def process(input: T)(implicit cc: CompilerContext): R
  // Dump pass results
  @nowarn("msg=parameter value cc .* is never used")
  protected def dump(result: R, tag: String)(implicit cc: CompilerContext): Unit = {}

  //////////////////////////////////////////////////////////////////////////////
  // Concrete members
  //////////////////////////////////////////////////////////////////////////////

  // Return single transformation that applies 'process' then 'dump'
  final def passTransformations: Iterator[(Any, Int, CompilerContext) => Option[R]] =
    Iterator single {
      case (input, passNumber, cc) =>
        lazy val tag = f"$passNumber%02d.$name"
        // Process the inputs
        val output = cc.timeit(f"pass $tag") {
          process(input.asInstanceOf[T])(cc)
        }
        // Dump result if requested
        if (cc.settings.dumpTrees) {
          dump(output, tag)(cc)
        }
        // Yield output, if there were no errors
        Option.unless(cc.hasError)(output): Option[R]
    }

}

// A pass representing the sequential application of two passes
final private class CombinedPass[T, R, U](a: Pass[T, R], b: Pass[R, U]) extends Pass[T, U] {
  // Transformations of a, then transformations of b
  def passTransformations: Iterator[(Any, Int, CompilerContext) => Option[Any]] =
    a.passTransformations concat b.passTransformations
}

trait PairsTransformerPass extends SimplePass[Pairs, Pairs] {

  final override def dump(result: Pairs, tag: String)(implicit cc: CompilerContext): Unit =
    result.asPar foreach { case (decl, defn) => cc.dump(decl, defn, "." + tag) }

}

abstract class PairTransformerPass extends PairsTransformerPass {

  // Called before any pair has been transformed with the input pairs
  @nowarn("msg=parameter value cc .* is never used")
  protected def start(pairs: Pairs)(implicit cc: CompilerContext): Unit = {}

  // Predicate to check whether this pair needs transforming
  @nowarn("msg=parameter value cc .* is never used")
  protected def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean = false

  // Called for each pair that needs to be transformed
  protected def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree)

  // Called after all pairs have been transformed with the output pairs
  @nowarn("msg=parameter value cc .* is never used")
  protected def finish(pairs: Pairs)(implicit cc: CompilerContext): Pairs = pairs

  // Implementation of the pass
  final protected def process(pairs: Pairs)(implicit cc: CompilerContext): Pairs = {
    // Call start
    start(pairs)

    // transform one pair, flatten Thicket/Stump
    def transformation(pair: (Decl, Defn)): Iterator[(Decl, Defn)] = pair match {
      case (decl, defn) if !skip(decl, defn) =>
        transform(decl, defn) match {
          case (newDecl: Decl, newDefn: Defn) => Iterator.single((newDecl, newDefn))
          case (Thicket(newDecls), Thicket(newDefns)) =>
            assert(newDecls.lengthIs == newDecls.length)
            assert(newDecls.forall(_.isInstanceOf[Decl]))
            assert(newDefns.forall(_.isInstanceOf[Defn]))
            (newDecls.iterator zip newDefns.iterator).asInstanceOf[Iterator[(Decl, Defn)]]
          case (Stump, Stump) => Iterator.empty
          case other          => throw Ice(s"Sadly, no. $other")
        }
      case pair => Iterator.single(pair)
    }

    // Apply the transformation
    val transformed = pairs.asPar.flatMap(transformation)

    // Call finish
    finish(transformed) tapEach {
      // Check pairs are consistent
      case (decl, defn) => assert(decl.symbol == defn.symbol, s"${decl.symbol} != ${defn.symbol}")
    }
  }

}

abstract class EntityTransformerPass(declFirst: Boolean) extends PairTransformerPass {

  // Only process entities
  final override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    decl match {
      case decl: DeclEntity => skip(decl, defn.asInstanceOf[DefnEntity])
      case _                => true
    }

  protected def skip(decl: DeclEntity, defn: DefnEntity): Boolean = false

  protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer

  final protected def transform(
      decl: Decl,
      defn: Defn
    )(
      implicit
      cc: CompilerContext
    ): (Tree, Tree) = {
    require(decl.symbol eq defn.symbol)
    val transformer = create(decl.symbol)
    @tailrec
    def loop(decl: Tree, defn: Tree, which: List[Boolean]): (Tree, Tree) = which match {
      case true :: tail  => loop(transformer(decl), defn, tail)
      case false :: tail => loop(decl, transformer(defn), tail)
      case Nil           => (decl, defn)
    }
    loop(decl, defn, List(declFirst, !declFirst))
  }

}
