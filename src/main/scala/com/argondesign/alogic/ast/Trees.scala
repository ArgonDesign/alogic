////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// AST representation used throughout the compiler
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.core.Locationed
import com.argondesign.alogic.core.SourceAttributes
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.StructuredTree
import com.argondesign.alogic.util.SequenceNumbers

// scalastyle:off number.of.types
// scalastyle:off number.of.methods

object Trees {

  private[this] val sequenceNumbers = new SequenceNumbers

  ///////////////////////////////////////////////////////////////////////////////
  // AST root type
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Tree extends StructuredTree with Locationed with TreeOps {
    val id = sequenceNumbers.next // TODO: Get rid of this
  }

  object Tree extends ObjectTreeOps

  ///////////////////////////////////////////////////////////////////////////////
  // Thicket
  ///////////////////////////////////////////////////////////////////////////////

  // Thicket is used where a node needs to be transformed into a list of nodes.
  // Thickets are transient and are flattened into the receiving list during
  // traversal. Any node type T that can be transformed into a Thicket must be
  // held as a List[T] by parent nodes and never as a simple T. These node
  // types include:
  //  - Stmt
  case class Thicket(trees: List[Tree]) extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // Root node for a file
  ///////////////////////////////////////////////////////////////////////////////

  case class Root(defs: List[Definition], entity: Entity) extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // References refer to definitions
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Ref extends Tree
  case class Ident(name: String) extends Ref with SourceAttributes
  case class Sym(symbol: Symbol) extends Ref

  ///////////////////////////////////////////////////////////////////////////////
  // Definition of types (except for entities)
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Definition extends Tree
  case class DefIdent(ident: Ident, kind: Type) extends Definition
  case class Def(symbol: TypeSymbol) extends Definition

  ///////////////////////////////////////////////////////////////////////////////
  // Declaration of terms (except for instances and functions)
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Declaration extends Tree
  case class DeclIdent(ident: Ident, kind: Type, init: Option[Expr]) extends Declaration
  case class Decl(symbol: TermSymbol, init: Option[Expr]) extends Declaration

  ///////////////////////////////////////////////////////////////////////////////
  // Entity (module) node
  ///////////////////////////////////////////////////////////////////////////////

  case class Entity(ref: Ref, body: List[Ent]) extends Tree with EntityOps

  ///////////////////////////////////////////////////////////////////////////////
  // Entity contents
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Ent extends Tree

  case class EntDecl(decl: Declaration) extends Ent
  case class EntEntity(entity: Entity) extends Ent
  case class EntInstance(
      instance: Ref,
      entity: Ref,
      paramNames: List[String],
      paramExprs: List[Expr]
  ) extends Ent
  case class EntConnect(lhs: Expr, rhs: List[Expr]) extends Ent
  case class EntCombProcess(stmts: List[Stmt]) extends Ent
  case class EntFunction(ref: Ref, stmts: List[Stmt]) extends Ent
  case class EntState(expr: Expr, stmts: List[Stmt]) extends Ent
  case class EntVerbatim(lang: String, body: String) extends Ent
  case class EntGen(gen: Gen) extends Ent
  // TODO: EntComment(str: String) extends Ent

  ///////////////////////////////////////////////////////////////////////////////
  // Gen constructs
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Gen extends Tree

  case class GenDecl(decl: Declaration) extends Gen
  case class GenIf(cond: Expr, thenItems: List[Tree], elseItems: List[Tree]) extends Gen
  case class GenFor(inits: List[Stmt], cond: Option[Expr], step: List[Stmt], body: List[Tree])
      extends Gen
  case class GenRange(decl: Declaration, op: String, end: Expr, body: List[Tree]) extends Gen

  ///////////////////////////////////////////////////////////////////////////////
  // Statements
  ///////////////////////////////////////////////////////////////////////////////

  // See note about Thicket
  sealed trait Stmt extends Tree

  case class StmtBlock(body: List[Stmt]) extends Stmt
  case class StmtIf(cond: Expr, thenStmts: List[Stmt], elseStmts: List[Stmt]) extends Stmt
  case class StmtCase(expr: Expr, cases: List[Case]) extends Stmt

  sealed trait Case extends Tree { val stmts: List[Stmt] }
  case class CaseRegular(cond: List[Expr], stmts: List[Stmt]) extends Case
  case class CaseDefault(stmts: List[Stmt]) extends Case
  case class CaseGen(gen: Gen) extends Case { val stmts = Nil }

  case class StmtLoop(body: List[Stmt]) extends Stmt
  case class StmtWhile(cond: Expr, body: List[Stmt]) extends Stmt
  case class StmtFor(inits: List[Stmt], cond: Option[Expr], step: List[Stmt], body: List[Stmt])
      extends Stmt
  case class StmtDo(cond: Expr, body: List[Stmt]) extends Stmt

  case class StmtLet(inits: List[Stmt], body: List[Stmt]) extends Stmt

  case class StmtFence() extends Stmt
  case class StmtBreak() extends Stmt
  case class StmtContinue() extends Stmt
  case class StmtGoto(expr: Expr) extends Stmt
  case class StmtReturn() extends Stmt

  case class StmtAssign(lhs: Expr, rhs: Expr) extends Stmt
  case class StmtUpdate(lhs: Expr, op: String, rhs: Expr) extends Stmt
  case class StmtPost(expr: Expr, op: String) extends Stmt
  case class StmtExpr(expr: Expr) extends Stmt
  case class StmtDecl(decl: Declaration) extends Stmt

  case class StmtRead() extends Stmt
  case class StmtWrite() extends Stmt

  case class StmtStall(cond: Expr) extends Stmt

  case class StmtComment(str: String) extends Stmt

  case class StmtGen(gen: Gen) extends Stmt

  case class StmtError() extends Stmt // placeholder when errors happened

  ///////////////////////////////////////////////////////////////////////////////
  // Expression nodes
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Expr extends Tree with ExprOps
  object Expr extends ObjectExprOps

  case class ExprCall(expr: Expr, args: List[Expr]) extends Expr

  // Operators
  case class ExprUnary(op: String, expr: Expr) extends Expr
  case class ExprBinary(lhs: Expr, op: String, rhs: Expr) extends Expr
  case class ExprTernary(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class ExprRep(count: Expr, expr: Expr) extends Expr
  case class ExprCat(parts: List[Expr]) extends Expr
  case class ExprIndex(expr: Expr, index: Expr) extends Expr
  case class ExprSlice(expr: Expr, lidx: Expr, op: String, ridx: Expr) extends Expr

  case class ExprSelect(expr: Expr, selector: String) extends Expr
  case class ExprIdent(name: String) extends Expr
  case class ExprRef(symbol: Symbol) extends Expr
  case class ExprType(kind: Type) extends Expr

  case class ExprCast(kind: Type, expr: Expr) extends Expr

  // Literals
  case class ExprInt(signed: Boolean, width: Int, value: BigInt) extends Expr {
    require(width > 0, s"widht=${width}")
    require(signed || value >= 0, s"signed=${signed}, value=${value}")
    require(if (value >= 0) (value >> width) == 0 else (value >> width) == -1,
            s"width=${width}, value=${value}")
  }
  case class ExprNum(signed: Boolean, value: BigInt) extends Expr {
    require(signed || value >= 0, s"signed=${signed}, value=${value}")
  }
  case class ExprStr(value: String) extends Expr

  case class ExprError() extends Expr // Placeholder when errors happened

}
