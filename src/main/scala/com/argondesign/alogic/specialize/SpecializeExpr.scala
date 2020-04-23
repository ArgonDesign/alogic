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
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

// Specialization result indicators
sealed trait ExprSpecialization
// Specialization failed due to error
case object ExprSpecializationError extends ExprSpecialization

// Specialization failed due to dependency on unresolved choice symbols
case class ExprSpecializationUnknown(symbols: collection.Set[Symbol]) extends ExprSpecialization {
  require(symbols forall { _.isChoice })
}

// Specialization successful with the given result
case class ExprSpecializationComplete(expr: Expr) extends ExprSpecialization

private[specialize] object SpecializeExpr {

  def apply(
      expr: Expr
    )(
      implicit
      cc: CompilerContext,
      specializeDesc: SpecializeDesc
    ): ExprSpecialization = {

    var hadError = false
    val unknowns = mutable.Set[Symbol]()

    val transform: StatefulTreeTransformer = new StatefulTreeTransformer {
      override val typed = false

      def error(loc: Loc, msg: String*): ExprError = {
        if (msg.nonEmpty) {
          cc.error(loc, msg: _*)
        }
        hadError = true
        ExprError() withLoc loc
      }

      def error(tree: Tree, msg: String*): ExprError = error(tree.loc, msg: _*)

      override def skip(tree: Tree): Boolean = tree match {
        // References to choice symbols are not specializable at this point
        case ExprSym(symbol) if symbol.isChoice => unknowns add symbol; true
        // References to dict symbols are not specializable at this point
        case ExprRef(Sym(symbol, idxs)) => assert(idxs.nonEmpty); unknowns add symbol; true
        // Skip references to parametrized types, these will now be specialized
        // if under a Call. Also skip references to already specialized symbols.
        case ExprSym(symbol) => symbol.isParametrized || symbol.isSpecialized
        // Stop if we have any unknowns or errors
        case _ => unknowns.nonEmpty || hadError
      }

      override def transform(tree: Tree): Tree = tree match {
        // TODO: Consider removing this to emit more errors in one go
        // Bail quickly if we know there are unknowns
        case _ if unknowns.nonEmpty => tree

        // Specialize references to non-specialized, non-parametric symbols.
        case ExprSym(symbol) =>
          specializeDesc(symbol.desc, ParamBindingsNamed(Map.empty), true, tree.loc) match {
            // If successful, replace with reference to specialized symbol
            case DescSpecializationComplete(decl, _, _) => ExprSym(decl.symbol) withLoc tree.loc
            case DescSpecializationPending(decl)        => ExprSym(decl.symbol) withLoc tree.loc
            // Propagate error
            case DescSpecializationErrorOther => error(tree)
            // Propagate unknown
            case DescSpecializationUnknown(symbols) => unknowns addAll symbols; tree
            // We used named bindings so this cannot happen ...
            case DescSpecializationErrorNeedsNamed => unreachable
          }

        // Specialize calls to 'int'/'uint' to the corresponding sized TypeInt
        case ExprCall(ExprType(TypeNum(signed)), args) =>
          lazy val hint = if (signed) "int" else "uint"
          args match {
            case List(ArgP(width)) =>
              if (!cc.typeCheck(width)) {
                // Width expression has a type error
                error(width)
              } else {
                // Width expression is well typed
                width.value match {
                  // Width is positive. Re-write as the sized integer type
                  case Some(v) if v > 0 => ExprType(TypeInt(signed, v)) withLoc tree.loc
                  // Width is not-positive
                  case Some(v) => error(width, s"Width of '$hint' must be positive (not $v)")
                  // Width is not known
                  case None => error(width, s"Width of '$hint' must be a compile time constant")
                }
              }
            case _ =>
              error(tree, s"Bad parameter to '$hint', a single positional argument is expected.")
          }

        // TODO: This needs to handle tgt == ExprSym vs tgt == ExprSel differently
        // Specialize types where used by the 'call' syntax
        case ExprCall(tgt, args) =>
          // Call target must be typeable at this point
          if (!cc.typeCheck(tgt)) {
            // Call target does not type check
            error(tgt)
          } else {
            tgt.tpe match {
              case _: TypeType => error(tgt, "Type does not take any parameters")
              case TypeParametrized(symbol) =>
                val (posArgs, namedArgs) = args partition {
                  case _: ArgP => true
                  case _: ArgN => false
                }
                if (posArgs.nonEmpty && namedArgs.nonEmpty) {
                  error(tree, "Mixing positional and named parameter assignments is not allowed")
                } else if (posArgs.lengthIs > 1) {
                  error(tree, "Multiple parameter assignments must be given by name")
                } else {
                  val bindings = if (posArgs.nonEmpty) {
                    ParamBindingsPositional {
                      posArgs collect { case ArgP(expr) => expr }
                    }
                  } else {
                    ParamBindingsNamed {
                      Map from {
                        namedArgs.iterator collect { case ArgN(name, expr) => name -> expr }
                      }
                    }
                  }
                  specializeDesc(symbol.desc, bindings, true, tree.loc) match {
                    // If successful, replace with reference to specialized symbol
                    case DescSpecializationComplete(decl, _, _) =>
                      ExprSym(decl.symbol) withLoc tree.loc
                    case DescSpecializationPending(decl) =>
                      ExprSym(decl.symbol) withLoc tree.loc
                    // Propagate Error
                    case _: DescSpecializationError => error(tree)
                    // Propagate Unknown
                    case DescSpecializationUnknown(symbols) => unknowns addAll symbols; tree
                  }
                }
              case _ => tree
            }
          }

        //
        case _ => tree
      }
    }

    val result = expr rewrite transform

    if (hadError) {
      ExprSpecializationError
    } else if (unknowns.nonEmpty) {
      ExprSpecializationUnknown(unknowns)
    } else {
      ExprSpecializationComplete(result)
    }
  }

}
