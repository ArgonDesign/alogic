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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

// format: off
// Specialization result indicators
private[specialize] sealed trait TypingSpecialization
// Specialization failed due to error
private[specialize] case object TypingSpecializationError extends TypingSpecialization
// Specialization failed due to dependency on un-expanded 'gen'
private[specialize] case object TypingSpecializationUnknown extends TypingSpecialization
// Specialization successful with the given result
private[specialize] case class TypingSpecializationComplete(decl: Decl, defn: Defn) extends TypingSpecialization
// format: on

private[specialize] object SpecializeTyping {

  def apply(
      desc: Desc
    )(
      implicit
      cc: CompilerContext,
      specializeDesc: SpecializeDesc
    ): TypingSpecialization = {
    require(desc.ref.idxs.isEmpty)
    require(!desc.isParametrized)

    def specializeExpr(
        expr: Expr
      )(
        f: Expr => TypingSpecialization
      ): TypingSpecialization = {
      SpecializeExpr(expr) match {
        case ExprSpecializationError       => TypingSpecializationError
        case _: ExprSpecializationUnknown  => TypingSpecializationUnknown
        case ExprSpecializationComplete(s) => f(s)
      }
    }

    def specializeDescs(
        descs: List[Desc]
      )(
        f: (Map[Symbol, Symbol], Iterator[Decl], Iterator[Defn]) => TypingSpecialization
      ): TypingSpecialization = {
      val unknown = descs exists {
        case _: DescChoice => true
        case _: DescParam  => unreachable
        case _             => false
      }

      if (unknown) {
        TypingSpecializationUnknown
      } else {
        val specialized = descs filter { !_.isParametrized } map { d =>
          d.symbol -> specializeDesc(d, ParamBindingsNamed(Map.empty), true, d.symbol.loc)
        }

        if (specialized exists { _._2 == DescSpecializationErrorOther }) {
          TypingSpecializationError
        } else if (specialized exists { _._2.isInstanceOf[DescSpecializationUnknown] }) {
          TypingSpecializationUnknown
        } else {
          // The symbol map
          val mapping = Map from {
            specialized.iterator map {
              case (s, DescSpecializationComplete(decl, _, _)) => s -> decl.symbol
              case _                                           => unreachable
            }
          }
          // Extra bits to add to the split Decl/Defn
          val extraDecls = specialized.iterator collect {
            case (_, DescSpecializationComplete(decl, _, _)) => decl
          }
          val extraDefns = specialized.iterator collect {
            case (_, DescSpecializationComplete(_, defn, _)) => defn
          }
          // Construct the result
          f(mapping, extraDecls, extraDefns)
        }
      }
    }

    def splice[T <: Tree, R <: Tree](trees: Iterator[T], f: T => R): Iterator[R] = trees map {
      tree =>
        f(tree) withLoc tree.loc
    }

    desc match {
      case DescVar(ref, spec, initOpt) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclVar(ref.symbol, specialSpec) withLoc desc.loc,
            DefnVar(ref.symbol, initOpt) withLoc desc.loc
          )
        }
      case DescVal(ref, spec, init) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclVal(ref.symbol, specialSpec) withLoc desc.loc,
            DefnVal(ref.symbol, init) withLoc desc.loc
          )
        }
      case DescIn(ref, spec, fc) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclIn(ref.symbol, specialSpec, fc) withLoc desc.loc,
            DefnIn(ref.symbol) withLoc desc.loc
          )
        }
      case DescOut(ref, spec, fc, st, initOpt) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclOut(ref.symbol, specialSpec, fc, st) withLoc desc.loc,
            DefnOut(ref.symbol, initOpt) withLoc desc.loc
          )
        }
      case DescPipeline(ref, spec) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclPipeline(ref.symbol, specialSpec) withLoc desc.loc,
            DefnPipeline(ref.symbol) withLoc desc.loc
          )
        }
      case _: DescParam => unreachable
      case DescConst(ref, spec, init) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclConst(ref.symbol, specialSpec) withLoc desc.loc,
            DefnConst(ref.symbol, init) withLoc desc.loc
          )
        }
      case DescGen(ref, spec, init) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclGen(ref.symbol, specialSpec) withLoc desc.loc,
            DefnGen(ref.symbol, init) withLoc desc.loc
          )
        }
      case DescArray(ref, elem, size) =>
        specializeExpr(elem) { specialElem =>
          specializeExpr(size) { specialSize =>
            TypingSpecializationComplete(
              DeclArray(ref.symbol, specialElem, specialSize) withLoc desc.loc,
              DefnArray(ref.symbol) withLoc desc.loc
            )
          }
        }
      case DescSram(ref, elem, size, st) =>
        specializeExpr(elem) { specialElem =>
          specializeExpr(size) { specialSize =>
            TypingSpecializationComplete(
              DeclSram(ref.symbol, specialElem, specialSize, st) withLoc desc.loc,
              DefnSram(ref.symbol) withLoc desc.loc
            )
          }
        }
      case DescType(ref, spec) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclType(ref.symbol, specialSpec) withLoc desc.loc,
            DefnType(ref.symbol) withLoc desc.loc
          )
        }
      case desc @ DescEntity(ref, _, _) =>
        specializeDescs(desc.descs) { (mapping, extraDecls, extraDefns) =>
          val decls = desc.decls ++ extraDecls
          val body = List from {
            desc.body.iterator filter {
              case _: EntDecl    => false
              case EntDesc(desc) => desc.isParametrized
              case _             => true
            } concat {
              splice(extraDefns, EntDefn)
            }
          }
          val decl = DeclEntity(ref.symbol, decls) withLoc desc.loc
          val defn = DefnEntity(ref.symbol, desc.variant, body) withLoc desc.loc
          TypingSpecializationComplete(decl, Replace(defn, mapping))
        }
      case desc @ DescRecord(ref, _) =>
        specializeDescs(desc.descs) { (mapping, extraDecls, extraDefns) =>
          val decls = desc.decls ++ extraDecls
          val body = List from {
            desc.body.iterator filter {
              case _: RecDecl    => false
              case RecDesc(desc) => desc.isParametrized
              case _             => true
            } concat {
              splice(extraDefns, RecDefn)
            }
          }
          val decl = DeclRecord(ref.symbol, decls) withLoc desc.loc
          val defn = DefnRecord(ref.symbol, body) withLoc desc.loc
          TypingSpecializationComplete(decl, Replace(defn, mapping))
        }
      case DescInstance(ref, spec) =>
        specializeExpr(spec) { specialSpec =>
          TypingSpecializationComplete(
            DeclInstance(ref.symbol, specialSpec) withLoc desc.loc,
            DefnInstance(ref.symbol) withLoc desc.loc
          )
        }
      case desc @ DescSingleton(ref, _, _) =>
        specializeDescs(desc.descs) { (mapping, extraDecls, extraDefns) =>
          val decls = desc.decls ++ extraDecls
          val body = List from {
            desc.body.iterator filter {
              case _: EntDecl    => false
              case EntDesc(desc) => desc.isParametrized
              case _             => true
            } concat {
              splice(extraDefns, EntDefn)
            }
          }
          val decl = DeclSingleton(ref.symbol, decls) withLoc desc.loc
          val defn = DefnSingleton(ref.symbol, desc.variant, body) withLoc desc.loc
          TypingSpecializationComplete(decl, Replace(defn, mapping))
        }
      case DescFunc(ref, variant, ret, args, body) =>
        specializeExpr(ret) { specialRet =>
          specializeDescs(args) { (mapping, extraDecls, extraDefns) =>
            val decl =
              DeclFunc(ref.symbol: Symbol, variant, specialRet, extraDecls.toList) withLoc desc.loc
            val defn = DefnFunc(ref.symbol: Symbol, extraDefns.toList, body) withLoc desc.loc
            TypingSpecializationComplete(decl, Replace(defn, mapping))
          }
        }
      case _: DescChoice => unreachable
    }
  }

}
