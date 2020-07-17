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
// Remove StmtDecl instances:
//  - Lift StmtDecl to entity Decls
//  - Insert local initializer
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.enums.UninitializedLocals
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable.ListBuffer
import scala.util.Random

final class ConvertLocalDecls(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private[this] val localDecls = ListBuffer[Decl]()

  private[this] val localDefns = ListBuffer[Defn]()

  private[this] var rng: Random = _

  override def start(tree: Tree): Unit = tree match {
    case Defn(symbol) => rng = new Random(symbol.name.foldLeft(0)(_ ^ _))
    case _: Decl      =>
    case _            => unreachable
  }

  private[this] def getDefaultInitializer(kind: Type): Option[Expr] = {
    val width = kind.width.toInt
    Option.when(width > 0 && cc.settings.uninitialized != UninitializedLocals.None) {
      val signed = kind.isSigned
      cc.settings.uninitialized match {
        case UninitializedLocals.None  => unreachable
        case UninitializedLocals.Zeros => ExprInt(signed, width, 0)
        case UninitializedLocals.Ones =>
          if (signed) {
            ExprInt(signed = true, width, -1)
          } else {
            ExprInt(signed = false, width, (BigInt(1) << width) - 1)
          }
        case UninitializedLocals.Random =>
          (width, signed) match {
            case (1, true)  => ExprInt(signed = true, 1, -BigInt(1, rng))
            case (1, false) => ExprInt(signed = false, 1, BigInt(1, rng))
            case (n, true)  => ExprInt(signed = true, n, BigInt(n, rng) - (BigInt(1) << (n - 1)))
            case (n, false) => ExprInt(signed = false, n, BigInt(n, rng))
          }
      }
    }
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Pull StmtDecl to entity
    ////////////////////////////////////////////////////////////////////////////

    case StmtDecl(decl: DeclVar) =>
      localDecls append decl
      Stump

    case StmtDecl(decl: DeclVal) =>
      localDecls append TypeAssigner(DeclVar(decl.symbol, decl.spec) withLoc decl.loc)
      Stump

    case _: StmtDecl => unreachable

    ////////////////////////////////////////////////////////////////////////////
    // Pull StmtDefn to entity and replace with assignment
    ////////////////////////////////////////////////////////////////////////////

    case StmtDefn(defn @ DefnVar(symbol, initOpt)) =>
      localDefns append TypeAssigner(DefnVar(symbol, None) withLoc defn.loc)
      initOpt orElse getDefaultInitializer(symbol.kind) match {
        case Some(init) => StmtAssign(ExprSym(symbol), init) regularize tree.loc
        case None       => Stump
      }

    case StmtDefn(defn @ DefnVal(symbol, init)) =>
      localDefns append TypeAssigner(DefnVar(symbol, None) withLoc defn.loc)
      StmtAssign(ExprSym(symbol), init) regularize tree.loc

    case _: StmtDefn => unreachable

    ////////////////////////////////////////////////////////////////////////////
    // Add entity extra Decl/Defn pairs
    ////////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity if localDecls.nonEmpty =>
      val newDecls = List.from(decl.decls.iterator ++ localDecls.iterator)
      TypeAssigner(decl.copy(decls = newDecls) withLoc tree.loc)

    case defn: DefnEntity if localDefns.nonEmpty =>
      val newDefns = localDefns.iterator map { d =>
        TypeAssigner(EntDefn(d) withLoc d.loc)
      }
      val newBody = List.from(defn.body.iterator ++ newDefns)
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: StmtDecl => throw Ice(node, "Local declaration remains")
    }
  }

}

object ConvertLocalDecls extends PairTransformerPass {
  val name = "convert-local-decls"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new ConvertLocalDecls
    // First transform the defn
    val newDefn = transformer(defn)
    // Then transform the decl
    val newDecl = transformer(decl)
    (newDecl, newDefn)
  }

}
