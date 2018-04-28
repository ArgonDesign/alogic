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

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Parser
import com.argondesign.alogic.transform.Regularize
import com.argondesign.alogic.util.unreachable

trait TreeOps extends TreePrintOps { this: Tree =>

  //////////////////////////////////////////////////////////////////////////////
  // Trees nodes have a type 'tpe' which can be set once
  //////////////////////////////////////////////////////////////////////////////

  final private[this] var _tpe: Type = null // scalastyle:ignore var.field

  final def hasTpe: Boolean = _tpe != null

  final def tpe: Type = if (hasTpe) _tpe else unreachable

  final def tpeOpt: Option[Type] = Option(_tpe)

  final def withTpe(kind: Type): this.type = {
    if (hasTpe) {
      unreachable
    } else {
      _tpe = kind
    }
    this
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Identity of this tree
  ////////////////////////////////////////////////////////////////////////////////

  final def id: Int = System.identityHashCode(this)

  ////////////////////////////////////////////////////////////////////////////////
  // Rewrie with TreeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  final def rewrite(tt: TreeTransformer): Tree = tt(this)

  ////////////////////////////////////////////////////////////////////////////////
  // Assign loc where missing
  ////////////////////////////////////////////////////////////////////////////////

  def assignLocs(loc: Loc)(implicit cc: CompilerContext): this.type = {
    val result = this rewrite new Regularize(loc, assignTypes = false)
    assert(result eq this)
    this
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Assign loc where missing and apply TypeAssigner were no type is set
  ////////////////////////////////////////////////////////////////////////////////

  def regularize(loc: Loc)(implicit cc: CompilerContext): this.type = {
    val result = this rewrite new Regularize(loc, assignTypes = true)
    assert(result eq this)
    this
  }
}

trait ObjectTreeOps extends TreeUntype {

  ////////////////////////////////////////////////////////////////////////////////
  // Implicit dispatchers for any Tree that can be directly parsed by the parser
  ////////////////////////////////////////////////////////////////////////////////

  implicit final val parseableRoot = new Parser.Parseable[Root] {
    type C = StartContext
    def parse(parser: AlogicParser): StartContext = parser.start()
    def build(ctx: StartContext)(implicit cc: CompilerContext): Root = RootBuilder(ctx)
  }

  implicit final val parseableTypeDefinition = new Parser.Parseable[TypeDefinition] {
    type C = Type_definitionContext
    def parse(parser: AlogicParser): Type_definitionContext = parser.type_definition()

    def build(ctx: Type_definitionContext)(implicit cc: CompilerContext): TypeDefinition =
      TypeDefinitionBuilder(ctx)
  }

  implicit final val parseableEntity = new Parser.Parseable[Entity] {
    type C = EntityContext
    def parse(parser: AlogicParser): EntityContext = parser.entity()
    def build(ctx: EntityContext)(implicit cc: CompilerContext): Entity = EntityBuilder(ctx)
  }

  implicit final val parseableDeclIdent = new Parser.Parseable[DeclIdent] {
    type C = DeclContext
    def parse(parser: AlogicParser): DeclContext = parser.decl()
    def build(ctx: DeclContext)(implicit cc: CompilerContext): DeclIdent = DeclBuilder(ctx)
  }

  implicit final val parseableConnect = new Parser.Parseable[Connect] {
    type C = ConnectContext
    def parse(parser: AlogicParser): ConnectContext = parser.connect()
    def build(ctx: ConnectContext)(implicit cc: CompilerContext): Connect = ConnectBuilder(ctx)
  }

  implicit final val parseableStmt = new Parser.Parseable[Stmt] {
    type C = StatementContext
    def parse(parser: AlogicParser): StatementContext = parser.statement()
    def build(ctx: StatementContext)(implicit cc: CompilerContext): Stmt = StmtBuilder(ctx)
  }

  implicit final val parseableExpr = new Parser.Parseable[Expr] {
    type C = ExprContext
    def parse(parser: AlogicParser): ExprContext = parser.expr()
    def build(ctx: ExprContext)(implicit cc: CompilerContext): Expr = ExprBuilder(ctx)
  }

}
