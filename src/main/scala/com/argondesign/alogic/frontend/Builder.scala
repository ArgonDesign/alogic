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
// The builder takes a parse tree and returns the corresponding AST.
// The builders are implemented using Antlr4 visitors this is just a small
// wrapper to invoke them on various kinds of parse trees.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.antlr.AlogicParser.BlockContext
import com.argondesign.alogic.antlr.AlogicParser.ConnectContext
import com.argondesign.alogic.antlr.AlogicParser.DeclContext
import com.argondesign.alogic.antlr.AlogicParser.EntityContext
import com.argondesign.alogic.antlr.AlogicParser.ExprContext
import com.argondesign.alogic.antlr.AlogicParser.StartContext
import com.argondesign.alogic.antlr.AlogicParser.StatementContext
import com.argondesign.alogic.antlr.AlogicParser.Type_definitionContext
import com.argondesign.alogic.antlr.ConnectBuilder
import com.argondesign.alogic.antlr.DeclBuilder
import com.argondesign.alogic.antlr.EntityBuilder
import com.argondesign.alogic.antlr.ExprBuilder
import com.argondesign.alogic.antlr.RootBuilder
import com.argondesign.alogic.antlr.StmtBuilder
import com.argondesign.alogic.antlr.TypeDefinitionBuilder
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable

import org.antlr.v4.runtime.ParserRuleContext

object Builder {

  abstract class Dispatcher[T <: ParserRuleContext, R <: Tree] {
    def dispatch(ctx: T)(implicit cc: CompilerContext): R
  }

  implicit val dispatcherRoot = new Dispatcher[StartContext, Root] {
    def dispatch(ctx: StartContext)(implicit cc: CompilerContext): Root = RootBuilder(ctx)
  }

  implicit val dispatcherEntity = new Dispatcher[EntityContext, Entity] {
    def dispatch(ctx: EntityContext)(implicit cc: CompilerContext): Entity = EntityBuilder(ctx)
  }

  implicit val dispatcherTypeDefinition = new Dispatcher[Type_definitionContext, TypeDefinition] {
    def dispatch(ctx: Type_definitionContext)(implicit cc: CompilerContext): TypeDefinition = TypeDefinitionBuilder(ctx)
  }

  implicit val dispatcherDecl = new Dispatcher[DeclContext, Decl] {
    def dispatch(ctx: DeclContext)(implicit cc: CompilerContext): Decl = DeclBuilder(ctx)
  }

  implicit val dispatcherConnect = new Dispatcher[ConnectContext, Connect] {
    def dispatch(ctx: ConnectContext)(implicit cc: CompilerContext): Connect = ConnectBuilder(ctx)
  }

  implicit val dispatcherStmtBlock = new Dispatcher[BlockContext, StmtBlock] {
    def dispatch(ctx: BlockContext)(implicit cc: CompilerContext): StmtBlock = {
      StmtBuilder(ctx) match {
        case block: StmtBlock => block
        case _                => unreachable
      }
    }
  }

  implicit val dispatcherStmt = new Dispatcher[StatementContext, Stmt] {
    def dispatch(ctx: StatementContext)(implicit cc: CompilerContext): Stmt = StmtBuilder(ctx)
  }

  implicit val dispatcherExpr = new Dispatcher[ExprContext, Expr] {
    def dispatch(ctx: ExprContext)(implicit cc: CompilerContext): Expr = ExprBuilder(ctx)
  }

  // Similarly to the parser, the Builder is polymorphic both in its input and output
  // to be able to build various kinds of trees directly
  def apply[T <: ParserRuleContext, R <: Tree](
    ctx: T
  )(implicit
    cc: CompilerContext,
    dispatcher: Dispatcher[T, R]
  ): R = {
    val tree = dispatcher.dispatch(ctx)

    // TODO: Make optional
    // Ensure all nodes have locations
    tree visitAll {
      case tree: Tree if !tree.hasLoc => cc.ice(s"Tree has no location $tree")
    }

    tree
  }
}
