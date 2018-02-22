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

import scala.collection.immutable.ListMap

import com.argondesign.alogic.core.Symbols.FuncSymbol
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types.Type

// scalastyle:off number.of.types
// scalastyle:off number.of.methods

object Trees {

  ///////////////////////////////////////////////////////////////////////////////
  // AST root type
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait Tree extends Product with Locationed

  ///////////////////////////////////////////////////////////////////////////////
  // Root node for a file
  ///////////////////////////////////////////////////////////////////////////////

  case class Root(typeDefinitions: List[TypeDefinition], entiy: EntityIdent) extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // Type definition nodes
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait TypeDefinition extends Tree

  case class TypeDefinitionStruct(name: String, fields: ListMap[String, Type]) extends TypeDefinition
  case class TypeDefinitionTypedef(name: String, kind: Type) extends TypeDefinition

  ///////////////////////////////////////////////////////////////////////////////
  // Entity (module) nodes
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait Entity extends Tree

  case class EntityIdent(
    // Entity being defined
    name: String,

    // Declarations in entity scope
    declarations: List[DeclIdent],

    // Instantiations
    instances: List[InstanceIdent],
    // Port connections
    connects: List[Connect],

    // Functions
    functions: List[FunctionIdent],
    // fence statements
    fenceStmts: List[Stmt],

    // Nested entity definitions
    entities: List[EntityIdent],

    // Verbatim sections. Map from language to string to insert into output
    verbatim: Map[String, String]
  ) extends Entity

  ///////////////////////////////////////////////////////////////////////////////
  // Entity contents
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait Decl extends Tree
  case class DeclIdent(name: String, kind: Type, init: Option[Expr]) extends Decl
  case class DeclSymbol(symbol: TermSymbol, kind: Type, init: Option[Expr]) extends Decl
  case class InstanceIdent(name: String, module: String, params: ListMap[String, Expr]) extends Tree
  case class Connect(lhs: Expr, rhs: List[Expr]) extends Tree
  case class FunctionIdent(name: String, body: List[Stmt]) extends Tree
  case class Function(symbol: FuncSymbol, body: List[CtrlStmt]) extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // Statements
  //
  // We have 3 kinds of statements. Eventually we want to end up with only
  // CtrlStmt and CombStmt instances, but we cannot disambiguate calls without
  // a symbol table during parsing, so we use plain Stmt nodes before we resolve
  // symbols
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait Stmt extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // Source Statements
  ///////////////////////////////////////////////////////////////////////////////

  case class StmtBlock(cmds: List[Stmt]) extends Stmt
  case class StmtIf(cond: Expr, body: Stmt, elsebody: Option[Stmt]) extends Stmt
  case class StmtCase(value: Expr, cases: List[StmtCaseClause], default: Option[Stmt]) extends Stmt
  case class StmtCaseClause(cond: List[Expr], body: Stmt) extends Tree

  case class StmtLoop(body: Stmt) extends Stmt
  case class StmtWhile(cond: Expr, body: Stmt) extends Stmt
  case class StmtFor(inits: List[Stmt], cond: Expr, incr: Stmt, body: Stmt) extends Stmt
  case class StmtDo(cond: Expr, body: Stmt) extends Stmt

  case class StmtLet(inits: List[Stmt], body: Stmt) extends Stmt

  case class StmtFence() extends Stmt
  case class StmtBreak() extends Stmt
  case class StmtCall(name: String) extends Stmt
  case class StmtGoto(name: String) extends Stmt
  case class StmtReturn() extends Stmt

  case class StmtAssign(lhs: Expr, rhs: Expr) extends Stmt
  case class StmtUpdate(lhs: Expr, op: String, rhs: Expr) extends Stmt
  case class StmtPost(lhs: Expr, op: String) extends Stmt
  case class StmtExpr(expr: Expr) extends Stmt
  case class StmtDecl(decl: Decl) extends Stmt

  case class StmtDollarComment(str: String) extends Stmt // TODO: remove

  ///////////////////////////////////////////////////////////////////////////////
  // Combinatorial statements
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait CombStmt extends Stmt

  case class CombBlock(cmds: List[CombStmt]) extends CombStmt
  case class CombIf(cond: Expr, body: CombStmt, elsebody: Option[CombStmt]) extends CombStmt
  case class CombCase(value: Expr, cases: List[CombCaseClause], default: Option[CombStmt]) extends CombStmt
  case class CombGoto(tgt: Int) extends CombStmt
  case class CombCall(tgt: Int, ret: Int) extends CombStmt
  case class CombReturn() extends CombStmt

  case class CombAssign(lhs: Expr, rhs: Expr) extends CombStmt
  case class CombUpdate(lhs: Expr, op: String, rhs: Expr) extends CombStmt
  case class CombPost(lhs: Expr, op: String) extends CombStmt
  case class CombExpr(expr: Expr) extends CombStmt
  case class CombDecl(decl: DeclSymbol) extends CombStmt

  case class CombDollarComment(str: String) extends CombStmt // TODO: remove

  case class CombCaseClause(cond: List[Expr], body: CombStmt) extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // Control statements
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait CtrlStmt extends Stmt

  case class CtrlBlock(cmds: List[Stmt]) extends CtrlStmt
  case class CtrlIf(cond: Expr, body: CtrlStmt, elsebody: Option[CtrlStmt]) extends CtrlStmt
  case class CtrlCase(value: Expr, cases: List[CtrlCaseClause], default: Option[CtrlStmt]) extends CtrlStmt
  case class CtrlGoto(symbol: FuncSymbol) extends CtrlStmt
  case class CtrlCall(symbol: FuncSymbol) extends CtrlStmt
  case class CtrlReturn() extends CtrlStmt

  case class CtrlLoop(body: CtrlBlock) extends CtrlStmt
  case class CtrlWhile(cond: Expr, body: List[Stmt]) extends CtrlStmt
  case class CtrlFor(init: CombStmt, cond: Expr, incr: CombStmt, body: List[Stmt]) extends CtrlStmt
  case class CtrlDo(cond: Expr, body: List[Stmt]) extends CtrlStmt
  case class CtrlFence() extends CtrlStmt
  case class CtrlBreak() extends CtrlStmt

  case class CtrlCaseClause(cond: List[Expr], body: CtrlStmt) extends Tree

  ///////////////////////////////////////////////////////////////////////////////
  // Error statement - placeholder when errors happened
  ///////////////////////////////////////////////////////////////////////////////

  case class StmtError() extends CtrlStmt with CombStmt

  ///////////////////////////////////////////////////////////////////////////////
  // Expression nodes
  ///////////////////////////////////////////////////////////////////////////////

  abstract sealed trait Expr extends Tree with ExprOps
  object Expr extends ObjectExprOps

  // TODO: Remove
  case class ExprBracket(content: Expr) extends Expr

  case class ExprCall(ref: Expr, args: List[Expr]) extends Expr

  // Operators
  case class ExprUnary(op: String, lhs: Expr) extends Expr
  case class ExprBinary(lhs: Expr, op: String, rhs: Expr) extends Expr
  case class ExprTernary(cond: Expr, lhs: Expr, rhs: Expr) extends Expr
  case class ExprRep(count: Expr, value: Expr) extends Expr
  case class ExprCat(parts: List[Expr]) extends Expr
  case class ExprIndex(ref: Expr, index: Expr) extends Expr
  case class ExprSlice(ref: Expr, lidx: Expr, op: String, ridx: Expr) extends Expr

  case class ExprSelect(prefix: Expr, selector: String) extends Expr

  case class ExprAtCall(name: String, args: List[Expr]) extends Expr
  case class ExprDollarCall(name: String, args: List[Expr]) extends Expr

  // Literals
  // Numbers have signedness and an optional width. If with is None, the number is considered un-sized
  case class ExprNum(signed: Boolean, width: Option[BigInt], value: BigInt) extends Expr
  case class ExprStr(value: String) extends Expr

  case class ExprIdent(name: String) extends Expr // A name as it is represented in source
  case class ExprSymbol(symbol: Symbol) extends Expr // ExprIdent after being resolved to a definition

  case class ExprError() extends Expr // Placeholder when errors happened

  ///////////////////////////////////////////////////////////////////////////////
  // Tree representing FSM states after control statement conversion
  ///////////////////////////////////////////////////////////////////////////////

  case class State(state: Int, contents: List[CombStmt]) extends Tree
}

// sealed trait DeclVerilog extends Decl
// case class DeclVerilogVar(kind: Type, id: String) extends DeclVerilog
// case class DeclVerilogArr(kind: ScalarType, id: String, dims: List[Expr]) extends DeclVerilog
//
//

//
// sealed trait StorageType extends StorageTypeOps
// case object StorageTypeWire extends StorageType
// case object StorageTypeBubble extends StorageType
// case object StorageTypeReg extends StorageType
