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
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

sealed trait ParamSubstitution

// Parameter substitution completed, desc is the substituted tree,
// unused is the unused parameter bindings
case class ParamSubstitutionComplete(desc: Desc, unused: ParamBindings) extends ParamSubstitution
// Parameter substitution failed due to unbound parameters with no default
// value, which are returned in 'unbound'
case class ParamSubstitutionUnbound(unbound: List[Desc]) extends ParamSubstitution
// Parameter substitution must be performed by name, but a positional binding
// was provided
case object ParamSubstitutionNeedsNamed extends ParamSubstitution

private[specialize] object SubstituteParams {

  // Substitute top level parameters given the bindings and convert them to
  // constants. Unbound parameters will be converted using their default
  // initializers if exists. Returns a ParameterSubstitution.
  def apply(
      desc: Desc,
      bindings: ParamBindings
    )(
      implicit
      cc: CompilerContext
    ): ParamSubstitution = {
    require(!desc.isInstanceOf[DescParam])
    require(!desc.isInstanceOf[DescParamType])

    def checkPosOk: Boolean = bindings match {
      case _: ParamBindingsPositional => desc.params.length == 1 && !desc.mayHaveGeneratedParam
      case _                          => true
    }

    if (!desc.isParametrized) {
      // If the input is not parametrized, it's pretty easy
      ParamSubstitutionComplete(desc, bindings)
    } else if (!checkPosOk) {
      // Fail if there is more than one parameter and positional bindings were provided
      ParamSubstitutionNeedsNamed
    } else {
      // Otherwise attempt to do some useful work

      // Compute the parameter bindings
      val pairs: List[(Desc, Option[Expr])] = {
        def getBinding(ref: Ref): Option[Expr] = bindings match {
          case ParamBindingsPositional(expr :: Nil) => Some(expr)
          case _: ParamBindingsPositional           => unreachable
          case ParamBindingsNamed(params) =>
            val (name, idxValues) = ref.symbol.attr.dictName.get match {
              case Some((name, idxValues)) => (name, idxValues)
              case _                       => (ref.symbol.name, Nil)
            }
            params.get((name, idxValues))
        }
        desc.params map {
          case desc @ DescParam(ref, _, default)  => desc -> (getBinding(ref) orElse default)
          case desc @ DescParamType(ref, default) => desc -> (getBinding(ref) orElse default)
          case _                                  => unreachable
        }
      }

      // Find the unbound parameters
      val unboundDescs = pairs collect {
        case (desc, None) => desc
      }

      if (unboundDescs.nonEmpty) {
        // Stop if unbound parameters exits and return their descs
        ParamSubstitutionUnbound(unboundDescs)
      } else {
        // Otherwise substitute the parameters

        // Compute the actual symbol bindings keyed by symbols (this is to
        // avoid replacing a parameter in a nested Desc with the same name)
        val symbolBindings: Map[Symbol, Expr] = Map from {
          pairs.iterator collect {
            case (desc, Some(expr)) => desc.symbol -> expr
          }
        }

        // Compute the unused bindings
        val unusedBindings = bindings match {
          case _: ParamBindingsPositional => ParamBindingsPositional(Nil)
          case ParamBindingsNamed(params) =>
            val unusedParams = params filterNot {
              case ((name, idxValues), _) =>
                symbolBindings.keysIterator exists { symbol =>
                  symbol.attr.dictName.get match {
                    case Some((sName, sIdxValues)) => sName == name && sIdxValues == idxValues
                    case None                      => symbol.name == name && idxValues.isEmpty
                  }
                }
            }
            ParamBindingsNamed(unusedParams)
        }

        // Transform that replaces the bound parameters
        val transform = new StatefulTreeTransformer {
          override val typed: Boolean = false

          override def transform(tree: Tree): Tree = tree match {
            // Change bound value parameters into constants
            case DescParam(ref @ Sym(symbol, _), spec, _) =>
              symbolBindings.get(symbol) match {
                case Some(init) =>
                  symbol.attr.wasParam set true
                  DescConst(ref, spec, init) withLoc tree.loc
                case None => tree
              }

            // Change bound type parameters into typedefs
            case DescParamType(ref @ Sym(symbol, _), _) =>
              symbolBindings.get(symbol) match {
                case Some(init) =>
                  symbol.attr.wasParam set true
                  DescType(ref, init) withLoc tree.loc
                case None => tree
              }

            //
            case _ => tree
          }
        }

        // Perform the transform
        val substituted = desc rewrite transform ensuring { _.params.isEmpty }

        // Done
        ParamSubstitutionComplete(substituted, unusedBindings)
      }
    }
  }

}
