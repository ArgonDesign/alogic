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
// Finalize specialization of entity
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

private[specialize] object Finalize {

  // Type all constants and simplify initializers (this also type checks the
  // final parameter assignments). Also typecheck typedefs that were created
  // for type parameters. Rename according to actual parameters.
  def apply(
      decl: Decl,
      defn: Defn
    )(
      implicit
      cc: CompilerContext
    ): Option[(Decl, Defn, Map[String, Either[BigInt, Type]])] = {
    require(decl.symbol eq defn.symbol)

    val nameComponents = mutable.ListBuffer[String](decl.symbol.name)

    var hadError = false

    val paramValues = mutable.Map[String, Either[BigInt, Type]]()

    val transform: StatefulTreeTransformer = new StatefulTreeTransformer {
      override val typed = false

      def typeCheck(tree: Tree): Boolean = cc.typeCheck(tree) tap {
        hadError |= !_
      }

      var declLevel = 0
      var defnLevel = 0

      override def skip(tree: Tree): Boolean = tree match {
        case desc: Desc => assert(desc.isParametrized); true
        case _          => false
      }

      override def enter(tree: Tree): Option[Tree] = {
        tree match {
          case _: Decl => declLevel += 1
          case _       =>
        }
        tree match {
          case _: Defn => defnLevel += 1
          case _       =>
        }
        None
      }

      def values(idxs: List[Expr])(implicit cc: CompilerContext): Option[List[BigInt]] =
        idxs match {
          case Nil => Some(Nil)
          case head :: tail =>
            val resOpt = values(tail) // Compute eagerly to emit all errors
            head.value match {
              case Some(value) => resOpt map { value :: _ }
              case None        => cc.error(head, "Identifier index must be a compile time constant"); None
            }
        }

      override def transform(tree: Tree): Tree = tree match {
        // Type check assigned type parameters
        case decl @ DeclType(symbol, spec) =>
          declLevel -= 1
          if (declLevel == 1 && symbol.attr.wasParam.isSet && typeCheck(decl)) {
            // Save final parameter value and add name component.
            val v = spec.tpe.asType.kind
            paramValues(symbol.name) = Right(v)
            nameComponents append {
              def typeToComp(kind: TypeFund): String = kind match {
                case TypeSInt(size)          => s"i$size"
                case TypeUInt(size)          => s"u$size"
                case TypeNum(true)           => "int"
                case TypeNum(false)          => "uint"
                case TypeVector(eType, size) => s"${typeToComp(eType)}_BAR_${size}_KET"
                case TypeVoid                => "void"
                case TypeStr                 => unreachable
                case TypeRecord(symbol, _)   => symbol.name
                case TypeEntity(symbol, _)   => symbol.name
              }
              s"${symbol.name}_${typeToComp(v)}"
            }
            // Remove attribute to reduce noise
            symbol.attr.wasParam.clear()
          }
          tree

        // Adjust declLevel
        case _: Decl =>
          declLevel -= 1
          tree

        // Transform assigned value parameters
        case defn @ DefnConst(symbol, _) =>
          defnLevel -= 1
          if (defnLevel == 1 && symbol.attr.wasParam.isSet && typeCheck(defn)) {
            // simplify initializer, in order to remove reference to
            // formal parameter expression
            // Note: symbol.init is not quite the 'init' from the DefnConst,
            // but has been normalized in the context of the Defn, so it can
            // now be fully evaluated even if it has a tick.
            val simplified = symbol.init.get.simplify
            // Save final parameter value and add name component.
            simplified.value foreach { v =>
              paramValues(symbol.name) = Left(v)
              nameComponents append s"${symbol.name}_$v"
            }
            // Remove attribute to reduce noise
            symbol.attr.wasParam.clear()
            // Replace initializer
            defn.copy(init = simplified) withLoc defn.loc
          } else {
            tree
          }

        // Adjust defnLevel
        case _: Defn =>
          defnLevel -= 1
          tree

        // Error for referencing x.p#[n] as x.p__n
        case ExprSel(ExprSym(iSymbol), sel, Nil) if iSymbol.kind.isEntity =>
          iSymbol.kind.asEntity.publicSymbols exists { pSymbol =>
            !pSymbol.attr.dictName.isSet && pSymbol.name == sel
          } pipe {
            case true => tree
            case false =>
              cc.error(tree, s"No port named '$sel' on instance '${iSymbol.name}'")
              hadError = true
              ExprError() withLoc tree.loc
          }

        case ExprSel(expr, sel, idxs) if idxs.nonEmpty =>
          val res = expr match {
            case ExprSym(iSymbol) if iSymbol.kind.isEntity =>
              values(idxs) map { idxValues =>
                iSymbol.kind.asEntity.publicSymbols collectFirst {
                  case portSymbol if portSymbol.attr.dictName.contains((sel, idxValues)) =>
                    ExprSel(expr, portSymbol.name, Nil)
                } getOrElse {
                  val srcName = idxValues.mkString(sel + "#[", ", ", "]")
                  cc.error(tree, s"No port named '$srcName' on instance '${iSymbol.name}'")
                  hadError = true
                  ExprError()
                }
              } getOrElse {
                ExprError()
              }
            case _ =>
              cc.error(tree, "Illegal use of '.' lookup with dictionary indices")
              hadError = true
              ExprError()
          }
          res withLoc tree.loc

        //
        case _ => tree
      }
    }

    val fDecl = decl rewrite transform
    val fDefn = defn rewrite transform

    // Rename based on actual parameter values
    fDecl.symbol.name = nameComponents mkString cc.sep

    if (hadError) None else Some((fDecl, fDefn, paramValues.toMap))
  }

}
