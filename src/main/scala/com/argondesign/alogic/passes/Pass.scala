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

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.util.ChainingSyntax

// The compiler pass interface
trait Pass[T, R] { self =>
  //////////////////////////////////////////////////////////////////////////////
  // Abstract members
  //////////////////////////////////////////////////////////////////////////////

  // Name of pass for debugging
  val name: String
  // The implementation of the pass transformation
  protected def process(input: T)(implicit cc: CompilerContext): R
  // Dump pass results // TODO: do with callback to factor out IO
  protected def dump(result: R, tag: String)(implicit cc: CompilerContext): Unit

  //////////////////////////////////////////////////////////////////////////////
  // Concrete members
  //////////////////////////////////////////////////////////////////////////////

  // How many complete transformations does this pass do
  protected val steps: Int = 1

  // Apply the pass
  final def apply(input: T)(implicit cc: CompilerContext): Option[R] = run(input, 0)

  // Run  the pass
  protected def run(input: T, passNumber: Int)(implicit cc: CompilerContext): Option[R] = {
    // Process the inputs
    val output = cc.timeit(f"pass $passNumber%02d $name")(process(input))
    // Dump result if requested
    if (cc.settings.dumpTrees) dump(output, f"$passNumber%02d.$name")
    // Yield output, if there were no errors
    if (cc.hasError) None else Some(output)
  }

  // Combine with subsequent pass, with this Pass applied first
  final def andThen[U](next: Pass[R, U]): Pass[T, U] = new Pass[T, U] {
    val name: String = self.name + " andThen " + next.name

    protected def process(input: T)(implicit cc: CompilerContext): U = unreachable

    protected def dump(result: U, tag: String)(implicit cc: CompilerContext): Unit = unreachable

    override protected val steps: Int = self.steps + next.steps

    override def run(input: T, passNumber: Int)(implicit cc: CompilerContext): Option[U] = {
      // Apply the self pass and then the tail pass
      self.run(input, passNumber) flatMap { next.run(_, passNumber + self.steps) }
    }

  }

}

// Passes before Elaborate work on a list of Root trees, together with a list
// of top level instance specifier expressions
trait PreElaboratePass extends Pass[(List[Root], List[Expr]), (List[Root], List[Expr])] {

  // Factory method to create a new instance of the tree transformer
  protected def create(implicit cc: CompilerContext): TreeTransformer

  protected def process(
      input: (List[Root], List[Expr])
    )(
      implicit
      cc: CompilerContext
    ): (List[Root], List[Expr]) =
    // Apply pass to all Roots, pass through top level specs
    (input._1 map { _ rewrite create }, input._2)

  final protected def dump(
      result: (List[Root], List[Expr]),
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit =
    result._1 foreach { cc.dump(_, "." + tag) }

}

trait PairsTransformerPass extends Pass[List[(Decl, Defn)], List[(Decl, Defn)]] {

  final protected def dump(
      result: List[(Decl, Defn)],
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit = {
    result foreach { case (decl, defn) => cc.dump(decl, defn, "." + tag) }
  }

}

trait PairTransformerPass extends PairsTransformerPass with ChainingSyntax {

  // Called before any pair has been transformed with the input pairs
  @nowarn("msg=parameter value cc .* is never used")
  protected def start(pairs: List[(Decl, Defn)])(implicit cc: CompilerContext): Unit = {}

  // Predicate to check whether this pair needs transforming
  @nowarn("msg=parameter value cc .* is never used")
  protected def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean = false

  // Called fore each pair that needs to be transformed
  protected def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree)

  // Called after all pairs have been transformed with the output pairs
  @nowarn("msg=parameter value cc .* is never used")
  protected def finish(
      pairs: List[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): List[(Decl, Defn)] = pairs

  // Implementation of the pass
  final protected def process(
      pairs: List[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): List[(Decl, Defn)] = {
    // Call start
    start(pairs)
    // Apply transform to all pairs, flatten Thicket/Stump
    val transformed = pairs flatMap {
      case (decl, defn) if !skip(decl, defn) =>
        transform(decl, defn) match {
          case (newDecl: Decl, newDefn: Defn) => Iterator.single((newDecl, newDefn))
          case (Thicket(newDecls), Thicket(newDefns)) =>
            assert(newDecls.lengthIs == newDecls.length)
            assert(newDecls forall { _.isInstanceOf[Decl] })
            assert(newDefns forall { _.isInstanceOf[Defn] })
            newDecls.asInstanceOf[List[Decl]] lazyZip newDefns.asInstanceOf[List[Defn]]
          case (Stump, Stump) => Nil
          case other          => cc.ice(s"Sadly, no. $other")
        }
      case pair => Iterator.single(pair)
    }
    // Check pairs are consistent
    transformed foreach {
      case (decl, defn) => assert(decl.symbol == defn.symbol, s"${decl.symbol} != ${defn.symbol}")
    }
    // Call finish
    finish(transformed) tapEach {
      // Ensure Decl/Defn are always in pairs
      case (decl, defn) =>
        val declSymbols = Set from {
          decl collect {
            case Decl(symbol) if symbol != decl.symbol => symbol
          }
        }
        val defnSymbols = Set from {
          defn collect {
            case EntDefn(Defn(symbol))                 => symbol // Do not recurse into sub symbols
            case RecDefn(Defn(symbol))                 => symbol // Do not recurse into sub symbols
            case Defn(symbol) if symbol != decl.symbol => symbol
          }
        }
        assert(declSymbols == defnSymbols)
    }
  }

}

abstract class EntityTransformerPass(declFirst: Boolean) extends PairTransformerPass {

  // Only process entities
  override protected def skip(
      decl: Decl,
      defn: Defn
    )(
      implicit
      cc: CompilerContext
    ): Boolean = decl match {
    case _: DeclEntity => false
    case _             => true
  }

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
