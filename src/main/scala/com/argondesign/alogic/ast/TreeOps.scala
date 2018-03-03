////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Common members of ast.Trees.Tree
// These are factored out into a separate file to keep ast.Trees readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.antlr.AlogicParser
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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.frontend.Parser

import Trees._

trait TreeOps { this: Tree =>
  ////////////////////////////////////////////////////////////////////////////////
  // Rewrie with TreeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  def rewrite(tt: TreeTransformer): Tree = tt(this)
}

trait ObjectTreeOps {

  ////////////////////////////////////////////////////////////////////////////////
  // Implicit dispatchers for any Tree that can be directly parsed by the parser
  ////////////////////////////////////////////////////////////////////////////////

  implicit val parseableRoot = new Parser.Parseable[Root] {
    type C = StartContext
    def parse(parser: AlogicParser): StartContext = parser.start()
    def build(ctx: StartContext)(implicit cc: CompilerContext): Root = RootBuilder(ctx)
  }

  implicit val parseableTypeDefinition = new Parser.Parseable[TypeDefinition] {
    type C = Type_definitionContext
    def parse(parser: AlogicParser): Type_definitionContext = parser.type_definition()
    def build(ctx: Type_definitionContext)(implicit cc: CompilerContext): TypeDefinition = TypeDefinitionBuilder(ctx)
  }

  implicit val parseableEntity = new Parser.Parseable[Entity] {
    type C = EntityContext
    def parse(parser: AlogicParser): EntityContext = parser.entity()
    def build(ctx: EntityContext)(implicit cc: CompilerContext): Entity = EntityBuilder(ctx)
  }

  implicit val parseableDecl = new Parser.Parseable[Decl] {
    type C = DeclContext
    def parse(parser: AlogicParser): DeclContext = parser.decl()
    def build(ctx: DeclContext)(implicit cc: CompilerContext): Decl = DeclBuilder(ctx)
  }

  implicit val parseableConnect = new Parser.Parseable[Connect] {
    type C = ConnectContext
    def parse(parser: AlogicParser): ConnectContext = parser.connect()
    def build(ctx: ConnectContext)(implicit cc: CompilerContext): Connect = ConnectBuilder(ctx)
  }

  implicit val parseableStmt = new Parser.Parseable[Stmt] {
    type C = StatementContext
    def parse(parser: AlogicParser): StatementContext = parser.statement()
    def build(ctx: StatementContext)(implicit cc: CompilerContext): Stmt = StmtBuilder(ctx)
  }

  implicit val parseableExpr = new Parser.Parseable[Expr] {
    type C = ExprContext
    def parse(parser: AlogicParser): ExprContext = parser.expr()
    def build(ctx: ExprContext)(implicit cc: CompilerContext): Expr = ExprBuilder(ctx)
  }

}
