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
// TreeUntype creates a deep copy of a tree, and removes all tpe annotations in
// the process
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

trait TreeUntype {

  def untype(tree: Tree): Tree = tree match {
    case node: EntityNamed           => untype(node)
    case node: EntityLowered         => untype(node)
    case node: Sym                   => untype(node)
    case node: Decl                  => untype(node)
    case node: Instance              => untype(node)
    case node: Connect               => untype(node)
    case node: Function              => untype(node)
    case node: State                 => untype(node)
    case node: Thicket               => untype(node)
    case node: RegularCase           => untype(node)
    case node: DefaultCase           => untype(node)
    case node: Expr                  => untype(node)
    case node: Stmt                  => untype(node)
    case node: TypeDefinitionTypedef => unreachable
    case node: TypeDefinitionStruct  => unreachable
    case node: Ident                 => unreachable
    case node: DeclIdent             => unreachable
    case node: Root                  => unreachable
    case node: EntityIdent           => unreachable
    case node: Gen                   => unreachable
  }

  def untype(tree: Stmt): Stmt = tree match {
    case node: StmtBlock    => untype(node)
    case node: StmtIf       => untype(node)
    case node: StmtCase     => untype(node)
    case node: StmtLoop     => untype(node)
    case node: StmtWhile    => untype(node)
    case node: StmtFor      => untype(node)
    case node: StmtDo       => untype(node)
    case node: StmtFence    => untype(node)
    case node: StmtBreak    => untype(node)
    case node: StmtContinue => untype(node)
    case node: StmtGoto     => untype(node)
    case node: StmtReturn   => untype(node)
    case node: StmtAssign   => untype(node)
    case node: StmtExpr     => untype(node)
    case node: StmtDecl     => untype(node)
    case node: StmtRead     => untype(node)
    case node: StmtWrite    => untype(node)
    case node: StmtComment  => untype(node)
    case node: StmtStall    => untype(node)
    case node: StmtError    => untype(node)
    case node: StmtLet      => unreachable
    case node: StmtUpdate   => unreachable
    case node: StmtPost     => unreachable
    case node: StmtGen      => unreachable
  }

  def untype(tree: Expr): Expr = tree match {
    case node: ExprCall    => untype(node)
    case node: ExprUnary   => untype(node)
    case node: ExprBinary  => untype(node)
    case node: ExprTernary => untype(node)
    case node: ExprRep     => untype(node)
    case node: ExprCat     => untype(node)
    case node: ExprIndex   => untype(node)
    case node: ExprSlice   => untype(node)
    case node: ExprSelect  => untype(node)
    case node: ExprRef     => untype(node)
    case node: ExprType    => untype(node)
    case node: ExprInt     => untype(node)
    case node: ExprNum     => untype(node)
    case node: ExprStr     => untype(node)
    case node: ExprError   => untype(node)
    case node: ExprCast    => untype(node)
    case node: ExprIdent   => unreachable
  }

  //////////////////////////////////////////////////////////////////////////////
  // Other
  //////////////////////////////////////////////////////////////////////////////

  def untype(node: EntityNamed): EntityNamed =
    node.copy(
      declarations = untype(node.declarations),
      instances = untype(node.instances),
      connects = untype(node.connects),
      fenceStmts = untype(node.fenceStmts),
      functions = untype(node.functions),
      states = untype(node.states),
      entities = untype(node.entities)
    ) withLoc node.loc

  def untype(node: EntityLowered): EntityLowered =
    node.copy(
      declarations = untype(node.declarations),
      instances = untype(node.instances),
      connects = untype(node.connects),
      statements = untype(node.statements)
    ) withLoc node.loc

  def untype(node: Ident): Ident = node.copy() withLoc node.loc

  def untype(node: Sym): Sym = node.copy() withLoc node.loc

  def untype(node: Decl): Decl =
    node.copy(
      init = untype(node.init)
    ) withLoc node.loc

  def untype(node: Instance): Instance =
    node.copy(
      ref = untype(node.ref.asInstanceOf[Sym]),
      module = untype(node.module.asInstanceOf[Sym]),
      paramExprs = untype(node.paramExprs)
    ) withLoc node.loc

  def untype(node: Connect): Connect =
    node.copy(
      lhs = untype(node.lhs),
      rhs = untype(node.rhs)
    ) withLoc node.loc

  def untype(node: Function): Function =
    node.copy(
      ref = untype(node.ref.asInstanceOf[Sym]),
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: State): State =
    node.copy(
      expr = untype(node.expr),
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: RegularCase): RegularCase =
    node.copy(
      cond = untype(node.cond),
      stmt = untype(node.stmt)
    ) withLoc node.loc

  def untype(node: DefaultCase): DefaultCase =
    node.copy(
      stmt = untype(node.stmt)
    ) withLoc node.loc

  def untype(node: Thicket): Thicket =
    node.copy(
      trees = untype(node.trees)
    ) withLoc node.loc

  //////////////////////////////////////////////////////////////////////////////
  // Stmt
  //////////////////////////////////////////////////////////////////////////////

  def untype(node: StmtBlock): StmtBlock =
    node.copy(
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: StmtIf): StmtIf =
    node.copy(
      cond = untype(node.cond),
      thenStmt = untype(node.thenStmt),
      elseStmt = untype(node.elseStmt)
    ) withLoc node.loc

  def untype(node: StmtCase): StmtCase =
    node.copy(
      expr = untype(node.expr),
      cases = untype(node.cases)
    ) withLoc node.loc

  def untype(node: StmtLoop): StmtLoop =
    node.copy(
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: StmtWhile): StmtWhile =
    node.copy(
      cond = untype(node.cond),
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: StmtFor): StmtFor =
    node.copy(
      inits = untype(node.inits),
      cond = untype(node.cond),
      step = untype(node.step),
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: StmtDo): StmtDo =
    node.copy(
      cond = untype(node.cond),
      body = untype(node.body)
    ) withLoc node.loc

  def untype(node: StmtFence): StmtFence = StmtFence() withLoc node.loc

  def untype(node: StmtBreak): StmtBreak = StmtBreak() withLoc node.loc

  def untype(node: StmtContinue): StmtContinue = StmtContinue() withLoc node.loc

  def untype(node: StmtGoto): StmtGoto =
    node.copy(
      expr = untype(node.expr)
    ) withLoc node.loc

  def untype(node: StmtReturn): StmtReturn = StmtReturn() withLoc node.loc

  def untype(node: StmtAssign): StmtAssign =
    node.copy(
      lhs = untype(node.lhs),
      rhs = untype(node.rhs)
    ) withLoc node.loc

  def untype(node: StmtExpr): StmtExpr =
    node.copy(
      expr = untype(node.expr)
    ) withLoc node.loc

  def untype(node: StmtDecl): StmtDecl =
    node.copy(
      decl = untype(node.decl.asInstanceOf[Decl])
    ) withLoc node.loc

  def untype(node: StmtRead): StmtRead = StmtRead() withLoc node.loc

  def untype(node: StmtWrite): StmtWrite = StmtWrite() withLoc node.loc

  def untype(node: StmtComment): StmtComment = node.copy() withLoc node.loc

  def untype(node: StmtStall): StmtStall =
    node.copy(cond = untype(node.cond)) withLoc node.loc

  def untype(node: StmtError): StmtError = StmtError() withLoc node.loc

  //////////////////////////////////////////////////////////////////////////////
  // Expr
  //////////////////////////////////////////////////////////////////////////////

  def untype(node: ExprCall): ExprCall =
    node.copy(
      expr = untype(node.expr),
      args = untype(node.args)
    ) withLoc node.loc

  def untype(node: ExprUnary): ExprUnary =
    node.copy(
      expr = untype(node.expr)
    ) withLoc node.loc

  def untype(node: ExprBinary): ExprBinary =
    node.copy(
      lhs = untype(node.lhs),
      rhs = untype(node.rhs)
    ) withLoc node.loc

  def untype(node: ExprTernary): ExprTernary =
    node.copy(
      cond = untype(node.cond),
      thenExpr = untype(node.thenExpr),
      elseExpr = untype(node.elseExpr)
    ) withLoc node.loc

  def untype(node: ExprRep): ExprRep =
    node.copy(
      count = untype(node.count),
      expr = untype(node.expr)
    ) withLoc node.loc

  def untype(node: ExprCat): ExprCat =
    node.copy(
      parts = untype(node.parts)
    ) withLoc node.loc

  def untype(node: ExprIndex): ExprIndex =
    node.copy(
      expr = untype(node.expr),
      index = untype(node.index)
    ) withLoc node.loc

  def untype(node: ExprSlice): ExprSlice =
    node.copy(
      expr = untype(node.expr),
      lidx = untype(node.lidx),
      ridx = untype(node.ridx)
    ) withLoc node.loc

  def untype(node: ExprSelect): ExprSelect =
    node.copy(
      expr = untype(node.expr)
    ) withLoc node.loc

  def untype(node: ExprCast): ExprCast =
    node.copy(
      expr = untype(node.expr)
    ) withLoc node.loc

  def untype(node: ExprRef): ExprRef = node.copy() withLoc node.loc

  def untype(node: ExprType): ExprType = node.copy() withLoc node.loc

  def untype(node: ExprInt): ExprInt = node.copy() withLoc node.loc

  def untype(node: ExprNum): ExprNum = node.copy() withLoc node.loc

  def untype(node: ExprStr): ExprStr = node.copy() withLoc node.loc

  def untype(node: ExprError): ExprError = ExprError() withLoc node.loc

  //////////////////////////////////////////////////////////////////////////////
  // List[Tree]
  //////////////////////////////////////////////////////////////////////////////

  def untype[T <: Tree](trees: List[T]): List[T] = {
    trees match {
      case head :: tail => {
        val newHead = untype(head).asInstanceOf[T]
        val newTail = untype[T](tail)
        if ((head eq newHead) && (tail eq newTail)) trees else newHead :: newTail
      }
      case Nil => Nil
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Option[Tree]
  //////////////////////////////////////////////////////////////////////////////

  def untype[T <: Tree](treeOpt: Option[T]): Option[T] = {
    treeOpt match {
      case Some(tree) => {
        val newTree = untype(tree).asInstanceOf[T]
        if (newTree eq tree) treeOpt else Some(newTree)
      }
      case None => treeOpt
    }
  }

}
