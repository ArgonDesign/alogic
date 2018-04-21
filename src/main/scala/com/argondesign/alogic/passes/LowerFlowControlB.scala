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
// Do:
// - Update Connects
// - Replace naked port references
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

final class LowerFlowControlB(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] var inConnect = false

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      // Update instance types to reflect added ports
      for (Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol), _, _) <- entity.instances) {
        iSymbol withDenot iSymbol.denot.copy(kind = eSymbol.denot.kind)
      }
    }

    case _: Connect => {
      inConnect = true
    }

    case _ => ()
  }

  // Given a symbol, return the corresponding payload symbol, if any
  private[this] def payloadSymbol(symbol: TermSymbol): Option[TermSymbol] = {
    symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
      case (pSymbol, _) => pSymbol
    } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
      case (pSymbol, _, _) => pSymbol
    } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
      case (pSymbol, _, _) => pSymbol
    } flatMap {
      case symbol: TermSymbol => Some(symbol)
      case ErrorSymbol        => None
    }
  }

  // Given a symbol, return the corresponding valid symbol, if any
  private[this] def validSymbol(symbol: TermSymbol): Option[TermSymbol] = {
    symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
      case (_, vSymbol) => vSymbol
    } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
      case (_, vSymbol, _) => vSymbol
    } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
      case (_, vSymbol, _) => vSymbol
    }
  }

  // Given a symbol, return the corresponding ready/accept symbol, if any
  private[this] def backSymbol(symbol: TermSymbol): Option[TermSymbol] = {
    symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
      case (_, _, rSymbol) => rSymbol
    } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
      case (_, _, aSymbol) => aSymbol
    }
  }

  // Replace a reference/port expression with an equivalent expression
  // that refers to the constituent symbol extracted by partSymbol
  private[this] def partExpr(
      expr: Expr,
      partSymbol: TermSymbol => Option[TermSymbol]
  ): Option[Expr] = expr match {
    case ExprRef(Sym(pSymbol: TermSymbol)) => {
      partSymbol(pSymbol) map { symbol =>
        ExprRef(Sym(symbol))
      }
    }
    case ExprSelect(ExprRef(Sym(iSymbol)), sel) => {
      val kind = iSymbol.denot.kind.asInstanceOf[TypeEntity]
      val pSymbolOpt = kind.portSymbols collectFirst {
        case symbol if symbol.name == sel && symbol.hasAttr("expanded-port") => symbol
      }
      pSymbolOpt flatMap partSymbol map { symbol =>
        assert(kind(symbol.name).isDefined)
        ExprSelect(ExprRef(Sym(iSymbol)), symbol.name)
      }
    }
    case _ => unreachable
  }

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Expressions
      //////////////////////////////////////////////////////////////////////////

      case expr: ExprRef if !inConnect => {
        // Rewrite references to ports with fc as references to the payload
        partExpr(expr, payloadSymbol) getOrElse tree
      }

      //////////////////////////////////////////////////////////////////////////
      // Connect
      //////////////////////////////////////////////////////////////////////////

      case Connect(lhs, rhss) => {
        // Expand inter-entity connections
        val pLhs = partExpr(lhs, payloadSymbol)
        val pRhss = rhss flatMap { partExpr(_, payloadSymbol) }
        assert(if (pLhs.isEmpty) pRhss.isEmpty else pRhss.length == rhss.length)

        val vLhs = partExpr(lhs, validSymbol)
        val vRhss = rhss flatMap { partExpr(_, validSymbol) }
        assert(if (vLhs.isEmpty) vRhss.isEmpty else vRhss.length == rhss.length)

        val bLhs = partExpr(lhs, backSymbol)
        val bRhss = rhss flatMap { partExpr(_, backSymbol) }
        assert(if (bLhs.isEmpty) bRhss.isEmpty else bRhss.length == rhss.length)
        assert(bLhs.isEmpty || rhss.length == 1)

        val pConn = pLhs map { lhs =>
          Connect(lhs, pRhss)
        }
        val vConn = vLhs map { lhs =>
          Connect(lhs, vRhss)
        }
        val bConn = bLhs map { rhs =>
          Connect(bRhss.head, List(rhs))
        }

        val newConns = {
          val it = pConn.toIterator ++ vConn.toIterator ++ bConn.toIterator
          it.toList
        }

        if (newConns.nonEmpty) Thicket(newConns) else tree
      } followedBy {
        inConnect = false
      }

      case _ => tree
    }

    // If we did modify the node, regularize it
    if (result ne tree) {
      result regularize tree.loc
    }

    // Done
    result
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(!inConnect)
  }

}
