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
import com.argondesign.alogic.passes.AddCasts
import com.argondesign.alogic.passes.ReplaceUnaryTicks
import com.argondesign.alogic.transform.Regularize
import com.argondesign.alogic.typer.ResolvePolyFunc
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions

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
  // Rewrite with TreeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  final def rewrite(tt: TreeTransformer): Tree = tt(this)

  ////////////////////////////////////////////////////////////////////////////
  // Regularize the tree
  ////////////////////////////////////////////////////////////////////////////

  def regularize(loc: Loc)(implicit cc: CompilerContext): this.type = {
    this rewrite new Regularize(loc)
    this
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Pretty print
  ////////////////////////////////////////////////////////////////////////////////

  def toPrettyString: String = {
    val sb = new StringBuilder
    var lvl = 0
    for (c <- this.toString) {
      sb append c
      c match {
        case '(' =>
          lvl += 1
          sb append ("\n" + "  " * lvl)
        case ')' =>
          lvl -= 1
        case ',' =>
          sb append ("\n" + "  " * lvl)
        case _ =>
      }
    }
    sb.toString
  }
}

//////////////////////////////////////////////////////////////////////////////
// Polymorphic extension methods on Trees
//////////////////////////////////////////////////////////////////////////////

final class TreeExt[T <: Tree](val tree: T) extends AnyVal {

  ////////////////////////////////////////////////////////////////////////////////
  // Bring tree into a normal form that can be directly evaluated
  ////////////////////////////////////////////////////////////////////////////////

  def normalize(implicit cc: CompilerContext): T = {
    val res = tree rewrite {
      new ReplaceUnaryTicks
    } rewrite {
      new ResolvePolyFunc
    } rewrite {
      new AddCasts
    }
    res.asInstanceOf[T]
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

  implicit final val parseableDefIdent = new Parser.Parseable[DefIdent] {
    type C = DefContext
    def parse(parser: AlogicParser): DefContext = parser.`def`()
    def build(ctx: DefContext)(implicit cc: CompilerContext): DefIdent = DefBuilder(ctx)
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

  implicit final val parseableEnt = new Parser.Parseable[Ent] {
    type C = Entity_contentContext
    def parse(parser: AlogicParser): Entity_contentContext = parser.entity_content()
    def build(ctx: Entity_contentContext)(implicit cc: CompilerContext): Ent = {
      val ents = EntBuilder(ctx)
      assert(ents.length == 1)
      ents.head
    }
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

  implicit final val parseableGen = new Parser.Parseable[Gen] {
    type C = GenerateContext
    def parse(parser: AlogicParser): GenerateContext = parser.generate()
    def build(ctx: GenerateContext)(implicit cc: CompilerContext): Gen = GenBuilder(ctx)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Tree to TreeExt implicit conversion
  //////////////////////////////////////////////////////////////////////////////

  implicit def tree2TreeExt[T <: Tree](tree: T) = new TreeExt[T](tree)
}
