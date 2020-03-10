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
// Transform parameters of definition to constants using given bindings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

import scala.util.chaining._

private[specialize] object Substitute {

  // Substitute top level parameters given the bindings and convert them to
  // constants. Unbound parameters will be converted using their default
  // initializers if exists. If an unbound parameter with no default initializer
  // exist, returns Left(list of unbound parameter descs). Otherwise returns
  // Right((substituted tree, unused bindings)).
  def apply(
      desc: Desc,
      bindings: Map[String, Expr]
  )(implicit cc: CompilerContext): Either[List[Desc], (Desc, Map[String, Expr])] = {
    require(!desc.isInstanceOf[DescParam])

    if (!desc.isParametrized) {
      // If the input is not parametrized, it's pretty easy
      Right((desc, bindings))
    } else {
      // Otherwise attempt to do some useful work

      // Get the default bindings
      val defaultBindings = Map from {
        desc.params collect {
          case DescParam(Sym(symbol, _), _, Some(init)) => symbol.name -> init
        }
      }

      // Augment the given bindings with the default parameters
      val completeBindings = defaultBindings ++ bindings

      // Find the unbound parameters with no default initializers
      val unboundDescs = desc.params filter { d =>
        !(completeBindings contains d.name)
      }

      if (unboundDescs.nonEmpty) {
        // Stop if unbound parameters exits and return their descs
        Left(unboundDescs)
      } else {
        // Otherwise substitute the parameters

        // Clone bound parameter symbols
        val symbolMap = Map from {
          desc.params collect {
            case d: Desc if completeBindings contains d.symbol.name => d.symbol -> d.symbol.dup
          }
        }

        // Compute the actual symbol bindings keyed by symbols (this is to
        // avoid replacing a parameter in a nested Desc with the same name),
        // also compute the unused bindings at the same time
        val (symbolBindings, unusedBindings) = {
          completeBindings map {
            // TODO: Use 'exists' instead of 'find'
            case (k, v) => (k, desc.params.find(_.symbol.name == k)) -> v
          } partition {
            case (k, _) => k._2.isDefined
          } pipe {
            case (used, unused) =>
              val sb = used map { case (k, v)   => symbolMap(k._2.get.symbol) -> v }
              val ub = unused map { case (k, v) => k._1 -> v }
              (sb, ub)
          }
        }

        // Transform that replaces the bound parameters
        val transform = new StatefulTreeTransformer {
          override val typed: Boolean = false

          override def transform(tree: Tree): Tree = tree match {
            // Replace cloned symbols
            case node: Sym =>
              symbolMap.get(node.symbol) match {
                case Some(symbol) => node.copy(symbol = symbol) withLoc node.loc
                case None         => tree
              }
            case node: ExprSym =>
              symbolMap.get(node.symbol) match {
                case Some(symbol) => node.copy(symbol = symbol) withLoc node.loc
                case None         => tree
              }

            // Change bound parameters into constants
            case DescParam(ref @ Sym(symbol, _), spec, _) =>
              symbolBindings.get(symbol) match {
                case Some(init) =>
                  symbol.attr.wasParam set true
                  DescConst(ref, spec, init) withLoc tree.loc
                case None => tree
              }

            //
            case _ => tree
          }
        }

        // Perform the transform
        val substituted = desc rewrite transform ensuring { !_.isParametrized }

        // Done
        Right((substituted, unusedBindings))
      }
    }
  }
}
