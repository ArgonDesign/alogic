////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Message

object ResolveNames {

  // Given a tree and a symbol table, resolve all named references in the tree
  // using the given symbol table, also turn ExprDot to ExprSel.
  def apply[T <: Tree](
      tree: T,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[T] = {
    val rAcc = Seq.newBuilder[Reason]
    val mAcc = Seq.newBuilder[Message]
    var resolvedSome = false

    object Transform extends StatelessTreeTransformer {
      override protected val typed: Boolean = false

      override protected def enter(tree: Tree): Option[Tree] = tree match {
        case ExprIdent(ident: Ident) =>
          Some {
            // Resolve names in the ident
            ResolveNames(ident, symtab) match {
              case Failure(ms) =>
                mAcc addAll ms
                tree
              case Unknown(rs) =>
                rAcc addAll rs
                tree
              case Partial(ident, rs) =>
                resolvedSome = true
                rAcc addAll rs
                ExprIdent(ident) withLocOf tree
              case Success(i) =>
                // Compute name of ident
                fe.nameFor(i.base, i.idxs) match {
                  case Failure(ms) =>
                    mAcc addAll ms
                    tree
                  case Unknown(rs) =>
                    rAcc addAll rs
                    tree
                  case Complete(name) =>
                    // Look up name in symbol table
                    symtab.get(name) match {
                      case SymbolTable.Defined(symbol) =>
                        resolvedSome = true
                        ExprSym(symbol) withLocOf tree
                      case SymbolTable.Undefined =>
                        rAcc addOne ReasonUnresolved(i)
                        tree
                    }
                }
            }
          }
        case _ => None
      }

      override protected def transform(tree: Tree): Tree = tree match {
        case ExprDot(tgt, base, idxs) if idxs.nonEmpty =>
          fe.nameFor(base, idxs) match {
            case Failure(ms) =>
              mAcc addAll ms
              tree
            case Unknown(rs) =>
              rAcc addAll rs
              tree
            case Complete(name) =>
              ExprDot(tgt, name, Nil) withLocOf tree
          }

        case _ => tree
      }
    }

    val result = tree rewrite Transform

    val ms = mAcc.result()
    if (ms.nonEmpty) {
      Failure(ms)
    } else {
      val rs = rAcc.result()
      if (rs.nonEmpty) {
        if (resolvedSome) {
          Partial(result, rs)
        } else {
          Unknown(rs)
        }
      } else {
        if (resolvedSome) {
          Complete(result)
        } else {
          Finished(result)
        }
      }
    }
  }

}
