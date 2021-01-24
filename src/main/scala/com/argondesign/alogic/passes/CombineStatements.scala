////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Attempt to combine statements so later passes can work on smaller trees
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeCopier
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

final class CombineStatements(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Pretend all calls have a side effect for this transform
  private def noSideEffect(as: Expr*): Boolean = as forall {
    _ forall { case _: ExprCall => false }
  }

  private def combine2(a: Stmt, b: Stmt): Option[Stmt] = (a, b) pipe {
    case (
          StmtAssign(
            aLhs @ ExprIndex(aLSub, aLExpr @ ExprInt(_, _, aLhsLIdx)),
            aRhs @ ExprIndex(aRSub, aRExpr @ ExprInt(_, _, aRhsLIdx))
          ),
          StmtAssign(
            ExprIndex(bLSub, bLExpr @ ExprInt(_, _, bLhsRIdx)),
            ExprIndex(bRSub, bRExpr @ ExprInt(_, _, bRhsRIdx))
          )
        )
        if aLSub == bLSub && aRSub == bRSub && bLhsRIdx == aLhsLIdx + 1 && bRhsRIdx == aRhsLIdx + 1 &&
          noSideEffect(aLSub, aRSub) =>
      // Combine
      //  x[n]   = y[m]
      //  x[n+1] = y[m+1]
      // Into:
      //  x[n+1:m] = y[m+1:m]
      Some {
        StmtAssign(
          TypeAssigner(ExprSlice(aLSub, bLExpr, ":", aLExpr) withLocOf aLhs).simplifyLValue,
          TypeAssigner(ExprSlice(aRSub, bRExpr, ":", aRExpr) withLocOf aRhs).simplify
        )
      }
    case (
          StmtAssign(
            aLhs @ ExprSlice(aLSub, ExprInt(_, _, aLhsLIdx), ":", _),
            aRhs @ ExprSlice(aRSub, ExprInt(_, _, aRhsLIdx), ":", _)
          ),
          StmtAssign(
            ExprIndex(bLSub, bLExpr @ ExprInt(_, _, bLhsRIdx)),
            ExprIndex(bRSub, bRExpr @ ExprInt(_, _, bRhsRIdx))
          )
        )
        if aLSub == bLSub && aRSub == bRSub && bLhsRIdx == aLhsLIdx + 1 && bRhsRIdx == aRhsLIdx + 1 &&
          noSideEffect(aLSub, aRSub) =>
      // Combine
      //  x[k:n] = y[l:m]
      //  x[k+1] = y[l+1]
      // Into:
      //  x[k+1:n] = y[l+1:m]
      Some {
        StmtAssign(
          TypeAssigner(aLhs.copy(lIdx = bLExpr) withLocOf aLhs).simplifyLValue,
          TypeAssigner(aRhs.copy(lIdx = bRExpr) withLocOf aRhs).simplify
        )
      }
    case (
          StmtAssign(
            aLhs @ ExprSlice(aLSub, ExprInt(_, _, aLhsLIdx), ":", aLhsRIdx),
            aRhs @ ExprSlice(aRSub, ExprInt(_, _, aRhsLIdx), ":", aRhsRIdx)
          ),
          StmtAssign(
            ExprSlice(bLSub, bLhsLIdx, ":", ExprInt(_, _, bLhsRIdx)),
            ExprSlice(bRSub, bRhsLIdx, ":", ExprInt(_, _, bRhsRIdx))
          )
        )
        if aLSub == bLSub && aRSub == bRSub && bLhsRIdx == aLhsLIdx + 1 && bRhsRIdx == aRhsLIdx + 1 &&
          noSideEffect(aLSub, aRSub, aLhsRIdx, aRhsRIdx, bLhsLIdx, bRhsLIdx) =>
      // Combine
      //  x[k:n]   = y[l:m]
      //  x[i:k+1] = y[j:l+1]
      // Into:
      //  x[i:n] = y[j:m]
      Some {
        StmtAssign(
          TypeAssigner(aLhs.copy(lIdx = bLhsLIdx) withLocOf aLhs).simplifyLValue,
          TypeAssigner(aRhs.copy(lIdx = bRhsLIdx) withLocOf aRhs).simplify
        )
      }
    case _ => None
  } map { combined =>
    TypeAssigner(combined withLocOf a)
  }

  private def combine(stmts: List[Stmt]): List[Stmt] = {
    val result = new ListBuffer[Stmt]

    @tailrec
    def loop(stmts: List[Stmt]): Unit = stmts match {
      case a :: (tail @ b :: ss) =>
        combine2(a, b) match {
          case Some(combined) => loop(combined :: ss)
          case None           => result.append(a); loop(tail)
        }
      case s :: ss => result.append(s); loop(ss)
      case Nil     =>
    }

    loop(stmts)

    if (result.lengthIs < stmts.length) result.toList else stmts
  }

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  // Transform every List[Stmt]
  override def transform(tree: Tree): Tree = tree pipe {
    case defn @ DefnFunc(_, args, body) =>
      TreeCopier(defn)(args, combine(body))

    case ent @ EntCombProcess(body) =>
      TreeCopier(ent)(combine(body))

    case stmt @ StmtBlock(body) =>
      TreeCopier(stmt)(combine(body))
    case stmt @ StmtIf(cond, thenStmts, elseStmts) =>
      TreeCopier(stmt)(cond, combine(thenStmts), combine(elseStmts))
    case stmt @ StmtLoop(body) =>
      TreeCopier(stmt)(combine(body))
    case stmt @ StmtWhile(cond, body) =>
      TreeCopier(stmt)(cond, combine(body))
    case stmt @ StmtFor(inits, condOpt, steps, body) =>
      TreeCopier(stmt)(inits, condOpt, steps, combine(body))
    case stmt @ StmtDo(cond, body) =>
      TreeCopier(stmt)(cond, combine(body))
    case stmt @ StmtLet(inits, body) =>
      TreeCopier(stmt)(inits, combine(body))

    case kase @ CaseRegular(cond, body) =>
      TreeCopier(kase)(cond, combine(body))
    case kase @ CaseDefault(body) =>
      TreeCopier(kase)(combine(body))

    // Node: There are more List[Stmt] in trees, but they cannot exist where this pass is run

    case _ => tree
  } match {
    case `tree` => tree
    case result => TypeAssigner(result)
  }

}

object CombineStatements extends PairTransformerPass {
  val name = "combine-statements"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (decl, cc.combineStatements(defn))
}
