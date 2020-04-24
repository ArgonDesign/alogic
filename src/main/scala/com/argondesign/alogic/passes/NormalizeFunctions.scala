////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
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

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeUInt
import com.argondesign.alogic.typer.TypeAssigner

final class NormalizeFunctions(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  private var selfSymbols: Option[(Symbol, Set[Symbol])] = None
  private var inStaticMethod: Boolean = false

  private def trimUnreachable[T <: Tree](stmts: List[Stmt])(copy: List[Stmt] => T): Option[T] = {
    val (init, tail) = stmts.iterator.span(!_.alwaysReturns)
    val lastOption = tail.nextOption // Need to consume this eagerly here
    val reachable = init concat lastOption
    Option.when(tail.nonEmpty) {
      cc.warning(tail.next, "Statement is unreachable")
      TypeAssigner(copy(reachable.toList))
    }
  }

  private def conditionalize(stmt: Stmt)(implicit didReturnSymbol: Symbol): Stmt = {
    require(!stmt.tpe.isCtrlStmt)
    stmt match {
      case s @ StmtBlock(body) =>
        val cbody = conditionalize(body)
        if (cbody eq body) {
          s
        } else {
          TypeAssigner(s.copy(cbody) withLoc s.loc)
        }

      case s @ StmtIf(_, ts, es) =>
        val cts = conditionalize(ts)
        val ces = conditionalize(es)
        if ((cts eq ts) && (ces eq es)) {
          s
        } else {
          TypeAssigner(s.copy(thenStmts = cts, elseStmts = ces) withLoc s.loc)
        }

      case s @ StmtCase(_, cases) =>
        val ccases = {
          def loop(cases: List[Case]): List[Case] = cases match {
            case head :: tail =>
              val chead = {
                val cstmts = conditionalize(head.stmts)
                if (cstmts eq head.stmts) {
                  head
                } else {
                  TypeAssigner(head.cpy(stmts = cstmts) withLoc head.loc)
                }
              }
              val ctail = loop(tail)
              if ((chead eq head) && (ctail eq tail)) cases else chead :: ctail
            case Nil => Nil
          }
          loop(cases)
        }
        if (ccases eq cases) s else TypeAssigner(s.copy(cases = ccases) withLoc s.loc)

      case _ => stmt
    }
  }

  // Conditionalize all statements which follow statements that might return.
  private def conditionalize(stmts: List[Stmt])(implicit didReturnSymbol: Symbol): List[Stmt] = {
    val (init, tail) = stmts.iterator span { _.neverReturns }
    if (tail.isEmpty) {
      stmts
    } else {
      val critical = tail.next
      assert(!critical.alwaysReturns || !tail.hasNext, "Unreachable should have been pruned")
      val fini = if (!tail.hasNext) {
        conditionalize(critical) :: Nil
      } else {
        // Note: This is purely a readability optimization: if the critical
        // statement is an 'if' with one branch always returning, then stick
        // the remaining statements at the end of the other branch.
        critical match {
          case s @ StmtIf(_, ts, es) if ts exists { _.alwaysReturns } =>
            conditionalize(
              TypeAssigner(s.copy(elseStmts = es appendedAll tail) withLoc s.loc)
            ) :: Nil
          case s @ StmtIf(_, ts, es) if es exists { _.alwaysReturns } =>
            conditionalize(
              TypeAssigner(s.copy(thenStmts = ts appendedAll tail) withLoc s.loc)
            ) :: Nil
          case _ =>
            // Generic rewrite
            val conditional = StmtIf(
              !ExprSym(didReturnSymbol),
              conditionalize(tail.toList),
              Nil
            ) regularize critical.loc
            conditionalize(critical) :: conditional :: Nil
        }
      }
      fini prependedAll init
    }
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: EntCombProcess => true
    case _                 => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    case defn: DefnRecord =>
      if (selfSymbols.nonEmpty) cc.ice(defn, "Nested structure definitions are not yet supported")
      selfSymbols = Some((defn.symbol, Set.from(defn.defns.iterator map { _.symbol })))
      None

    case defn: DefnFunc =>
      assert(!inStaticMethod)
      inStaticMethod = defn.symbol.kind.isStaticMethod
      None

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Normalize method/combinational function bodies
    ////////////////////////////////////////////////////////////////////////////

    case defn @ DefnFunc(symbol, _, body) if !symbol.kind.isXenoFunc =>
      inStaticMethod = false

      val trimmed = trimUnreachable(body) { reachable =>
        defn.copy(body = reachable) withLoc tree.loc
      } getOrElse defn

      val hadError = !symbol.kind.asCallable.retType.isVoid && {
        trimmed.body.lastOption pipe {
          case Some(last) => Option.when(!last.alwaysReturns)(last)
          case None       => Some(tree)
        } tap {
          case Some(loc) => cc.error(loc, "Control reaches end of non-void function")
          case None      => // OK
        } pipe {
          _.isDefined
        }
      }

      if (hadError) {
        tree
      } else if (symbol.kind.isCtrlFunc || (trimmed.body.init forall { _.neverReturns })) {
        trimmed
      } else {
        // This is a combinational function and an intermediate statement
        // exists that might return, need to rewrite body to disable effects
        // after a return has been executed

        // The flag noting a return has occurred
        val didReturnSymbol = cc.newSymbol("_didReturn", tree.loc) tap { _.kind = TypeUInt(1) }

        // Initial statements (decl/defn for above, initialized to 0)
        val init = {
          val decl = StmtDecl(didReturnSymbol.mkDecl) regularize tree.loc
          val defn = StmtDefn(didReturnSymbol.mkDefn(ExprInt(false, 1, 0))) regularize tree.loc
          Iterator(decl, defn)
        }

        // Transform that sets the flag on return
        object Transform extends StatelessTreeTransformer {
          override def transform(tree: Tree): Tree = tree match {
            case stmt: StmtReturn =>
              val setDidReturn =
                StmtAssign(ExprSym(didReturnSymbol), ExprInt(false, 1, 1)) regularize stmt.loc
              Thicket(List(setDidReturn, stmt))
            case _ => tree
          }
        }

        val newBody =
          conditionalize((trimmed rewrite Transform).body)(didReturnSymbol) prependedAll init

        TypeAssigner(trimmed.copy(body = newBody) withLoc trimmed.loc)
      }

    ////////////////////////////////////////////////////////////////////////////
    // Pop selfSymbols
    ////////////////////////////////////////////////////////////////////////////

    case _: DefnRecord =>
      selfSymbols = None
      tree

    ////////////////////////////////////////////////////////////////////////////
    // Need to check every List[Stmt] as well for unreachable code
    ////////////////////////////////////////////////////////////////////////////

    case stmt @ StmtBlock(body) =>
      trimUnreachable(body) { reachable =>
        stmt.copy(body = reachable) withLoc tree.loc
      } getOrElse tree

    case stmt @ StmtIf(_, ts, _) =>
      trimUnreachable(ts) { reachable =>
        stmt.copy(thenStmts = reachable) withLoc tree.loc
      } getOrElse stmt pipe {
        case stmt @ StmtIf(_, _, es) =>
          trimUnreachable(es) { reachable =>
            stmt.copy(elseStmts = reachable) withLoc tree.loc
          } getOrElse stmt
      }

    case stmt @ StmtLoop(body) =>
      trimUnreachable(body) { reachable =>
        stmt.copy(body = reachable) withLoc tree.loc
      } getOrElse tree

    case stmt @ StmtWhile(_, body) =>
      trimUnreachable(body) { reachable =>
        stmt.copy(body = reachable) withLoc tree.loc
      } getOrElse tree

    case stmt @ StmtFor(_, _, _, body) =>
      trimUnreachable(body) { reachable =>
        stmt.copy(body = reachable) withLoc tree.loc
      } getOrElse tree

    case stmt @ StmtDo(_, body) =>
      trimUnreachable(body) { reachable =>
        stmt.copy(body = reachable) withLoc tree.loc
      } getOrElse tree

    // StmtLet removed in Desugar

    case kase @ CaseRegular(_, stmts) =>
      trimUnreachable(stmts) { reachable =>
        kase.copy(stmts = reachable) withLoc tree.loc
      } getOrElse tree

    case kase @ CaseDefault(stmts) =>
      trimUnreachable(stmts) { reachable =>
        kase.copy(stmts = reachable) withLoc tree.loc
      } getOrElse tree

    ////////////////////////////////////////////////////////////////////////////
    // Replace references to selfSymbols with a 'this' reference.
    ////////////////////////////////////////////////////////////////////////////

    case ExprSym(symbol) if !symbol.kind.isStaticMethod =>
      selfSymbols match {
        case Some((thisSymbol, selfSymbols)) if selfSymbols(symbol) =>
          if (inStaticMethod) {
            cc.error(tree, "Static method cannot reference non-static members")
            tree
          } else {
            (ExprThis(ExprSym(thisSymbol)) select symbol.name) regularize tree.loc
          }
        case _ => tree
      }

    case _ => tree
  }

}

object NormalizeFunctions extends PairTransformerPass {
  val name = "normalize-functions"

  final protected def transform(
      decl: Decl,
      defn: Defn
    )(
      implicit
      cc: CompilerContext
    ): (Tree, Tree) = {
    require(decl.symbol eq defn.symbol)
    val transformer = new NormalizeFunctions
    (transformer(decl), transformer(defn))
  }

}
