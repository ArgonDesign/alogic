////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2019 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.core.FlowControlTypes.FlowControlType
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Locationed
import com.argondesign.alogic.core.SourceAttribute
import com.argondesign.alogic.core.StorageTypes.StorageType
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.SequenceNumbers

import scala.collection.mutable

object Trees {

  private[this] val sequenceNumbers = new SequenceNumbers

  //////////////////////////////////////////////////////////////////////////////
  // AST base type
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Tree extends TreeImpl with TreeOps {
    val id: Int = sequenceNumbers.next // TODO: Get rid of this
  }

  object Tree extends ObjectTreeOps

  //////////////////////////////////////////////////////////////////////////////
  // Root node (contents of a file)
  //////////////////////////////////////////////////////////////////////////////

  case class Root(body: List[Riz]) extends Tree with RootOps

  //////////////////////////////////////////////////////////////////////////////
  // Ref (reference) nodes refer to names/symbols prior to elaboration
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Ref extends Tree with RefOps { val idxs: List[Expr] }

  case class Ident(override val name: String, idxs: List[Expr]) extends Ref with IdentImpl
  case class Sym(override val symbol: Symbol, idxs: List[Expr]) extends Ref

  //////////////////////////////////////////////////////////////////////////////
  // Desc (description) nodes introduce names (symbols) as in source code,
  // they only exist prior to elaboration, at which point they are split into
  // pairs of Decl/Defn nodes
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Desc extends Tree with DescImpl with DescOps { val ref: Ref }

  object Desc extends DescObjOps
  
  case class DescVar(ref: Ref, spec: Expr, initOpt: Option[Expr]) extends Desc
  case class DescVal(ref: Ref, spec: Expr, init: Expr) extends Desc
  case class DescIn(ref: Ref, spec: Expr, fc: FlowControlType) extends Desc
  case class DescOut(ref: Ref, spec: Expr, fc: FlowControlType, st: StorageType, initOpt: Option[Expr]) extends Desc
  case class DescPipeline(ref: Ref, spec: Expr) extends Desc
  case class DescParam(ref: Ref, spec: Expr, initOpt: Option[Expr]) extends Desc
  case class DescConst(ref: Ref, spec: Expr, init: Expr) extends Desc
  case class DescGen(ref: Ref, spec: Expr, init: Expr) extends Desc
  case class DescArray(ref: Ref, elem: Expr, size: Expr) extends Desc
  case class DescSram(ref: Ref, elem: Expr, size: Expr, st: StorageType) extends Desc
//  case class DescStack(ref: Ref, elem: Expr, size: Expr) extends Desc
  case class DescType(ref: Ref, spec: Expr) extends Desc
  case class DescEntity(ref: Ref, variant: EntityVariant.Type, body: List[Ent]) extends Desc with DescEntityOps
  case class DescRecord(ref: Ref, body: List[Rec]) extends Desc with DescRecordOps
  case class DescInstance(ref: Ref, spec: Expr) extends Desc
  case class DescSingleton(ref: Ref, variant: EntityVariant.Type, body: List[Ent]) extends Desc with DescSingletonOps
  case class DescFunc(ref: Ref, variant: FuncVariant, ret: Expr, args: List[Desc], body: List[Stmt]) extends Desc
//  case class DescState(ref: Ref, expr: Expr, body: List[Stmt]) extends Desc
  case class DescChoice(ref: Ref, choices: List[ExprSym]) extends Desc
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Decl (declaration) nodes specify the type of symbols
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Decl extends Tree with DeclImpl with DeclOps { val symbol: Symbol }

  object Decl extends DeclObjOps

  case class DeclVar(symbol: Symbol, spec: Expr) extends Decl
  case class DeclVal(symbol: Symbol, spec: Expr) extends Decl
  case class DeclIn(symbol: Symbol, spec: Expr, fc: FlowControlType) extends Decl
  case class DeclOut(symbol: Symbol, spec: Expr, fc: FlowControlType, st: StorageType) extends Decl
  case class DeclPipeline(symbol: Symbol, spec: Expr) extends Decl
//  case class DeclParam(symbol: Symbol, spec: Expr) extends Decl
  case class DeclConst(symbol: Symbol, spec: Expr) extends Decl
  case class DeclGen(symbol: Symbol, spec: Expr) extends Decl
  case class DeclArray(symbol: Symbol, elem: Expr, size: Expr) extends Decl
  case class DeclSram(symbol: Symbol, elem: Expr, size: Expr, st: StorageType) extends Decl
  case class DeclStack(symbol: Symbol, elem: Expr, size: Expr) extends Decl
  case class DeclType(symbol: Symbol, spec: Expr) extends Decl
  case class DeclEntity(symbol: Symbol, override val decls: List[Decl]) extends Decl with DeclEntityOps
  case class DeclRecord(symbol: Symbol, override val decls: List[Decl]) extends Decl with DeclRecordOps
  case class DeclInstance(symbol: Symbol, spec: Expr) extends Decl
  case class DeclSingleton(symbol: Symbol, override val decls: List[Decl]) extends Decl with DeclSingletonOps
  case class DeclFunc(symbol: Symbol, variant: FuncVariant, ret: Expr, args: List[Decl]) extends Decl
  case class DeclState(symbol: Symbol) extends Decl
//  case class DeclChoice(symbol: Symbol) extends Decl
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Defn (definition) nodes specify the implementation of symbols
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Defn extends Tree with DefnImpl with DefnOps { val symbol: Symbol }

  object Defn extends DefnObjOps
  
  case class DefnVar(symbol: Symbol, initOpt: Option[Expr]) extends Defn
  case class DefnVal(symbol: Symbol, init: Expr) extends Defn
  case class DefnIn(symbol: Symbol) extends Defn
  case class DefnOut(symbol: Symbol, initOpt: Option[Expr]) extends Defn
  case class DefnPipeline(symbol: Symbol) extends Defn
//  case class DefnParam(symbol: Symbol, initOpt: Option[Expr]) extends Defn
  case class DefnConst(symbol: Symbol, init: Expr) extends Defn
  case class DefnGen(symbol: Symbol, init: Expr) extends Defn
  case class DefnArray(symbol: Symbol) extends Defn
  case class DefnSram(symbol: Symbol) extends Defn
  case class DefnStack(symbol: Symbol) extends Defn
  case class DefnType(symbol: Symbol) extends Defn
  case class DefnEntity(symbol: Symbol, variant: EntityVariant.Type, body: List[Ent]) extends Defn with DefnEntityOps
  case class DefnRecord(symbol: Symbol, body: List[Rec]) extends Defn with DefnRecordOps
  case class DefnInstance(symbol: Symbol) extends Defn
  case class DefnSingleton(symbol: Symbol, variant: EntityVariant.Type, body: List[Ent]) extends Defn with DefnSingletonOps
  case class DefnFunc(symbol: Symbol, args: List[Defn], body: List[Stmt]) extends Defn with DefnFuncOps
  case class DefnState(symbol: Symbol, expr: Expr, body: List[Stmt]) extends Defn
//  case class DefnChoice(symbol: Symbol, choices: List[ExprSym]) extends Defn
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Gen constructs
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Gen extends Tree

  case class GenIf(cond: Expr, thenItems: List[Tree], elseItems: List[Tree]) extends Gen
  case class GenFor(inits: List[Stmt], cond: Expr, steps: List[Stmt], body: List[Tree]) extends Gen
  case class GenRange(inits: List[Stmt], op: String, end: Expr, body: List[Tree]) extends Gen
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Assertion
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Assertion extends Tree

  case class AssertionAssert(cond: Expr, msgOpt: Option[String]) extends Assertion
  case class AssertionAssume(cond: Expr, msgOpt: Option[String]) extends Assertion
  case class AssertionStatic(cond: Expr, msgOpt: Option[String]) extends Assertion

  //////////////////////////////////////////////////////////////////////////////
  // Root contents (Riz is from 'riza', the Greek word for root)
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Riz extends Tree

  case class RizDesc(desc: Desc) extends Riz
  case class RizDecl(decl: Decl) extends Riz
  case class RizDefn(defn: Defn) extends Riz

  //////////////////////////////////////////////////////////////////////////////
  // Entity contents
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Ent extends Tree

  case class EntDesc(desc: Desc) extends Ent
  case class EntDecl(decl: Decl) extends Ent
  case class EntDefn(defn: Defn) extends Ent
  case class EntGen(gen: Gen) extends Ent
  case class EntConnect(lhs: Expr, rhs: List[Expr]) extends Ent
  case class EntCombProcess(stmts: List[Stmt]) extends Ent
  case class EntClockedProcess(clk: Expr, rstOpt: Option[Expr], stmts: List[Stmt]) extends Ent
  case class EntAssertion(assertion: Assertion) extends Ent
  case class EntVerbatim(lang: String, body: String) extends Ent
  case class EntComment(str: String) extends Ent

  //////////////////////////////////////////////////////////////////////////////
  // Record contents
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Rec extends Tree

  case class RecDesc(desc: Desc) extends Rec
  case class RecDecl(decl: Decl) extends Rec
  case class RecDefn(defn: Defn) extends Rec
  case class RecGen(gen: Gen) extends Rec
  case class RecAssertion(assertion: Assertion) extends Rec
  case class RecComment(str: String) extends Rec

  //////////////////////////////////////////////////////////////////////////////
  // Statements
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Stmt extends Tree with StmtOps

  case class StmtDesc(desc: Desc) extends Stmt
  case class StmtDecl(decl: Decl) extends Stmt
  case class StmtDefn(defn: Defn) extends Stmt
  case class StmtGen(gen: Gen) extends Stmt
  case class StmtBlock(body: List[Stmt]) extends Stmt
  case class StmtIf(cond: Expr, thenStmts: List[Stmt], elseStmts: List[Stmt]) extends Stmt
  case class StmtCase(expr: Expr, cases: List[Case]) extends Stmt
  case class StmtLoop(body: List[Stmt]) extends Stmt
  case class StmtWhile(cond: Expr, body: List[Stmt]) extends Stmt
  case class StmtFor(inits: List[Stmt], cond: Option[Expr], steps: List[Stmt], body: List[Stmt]) extends Stmt
  case class StmtDo(cond: Expr, body: List[Stmt]) extends Stmt
  case class StmtLet(inits: List[Stmt], body: List[Stmt]) extends Stmt
  case class StmtFence() extends Stmt
  case class StmtBreak() extends Stmt
  case class StmtContinue() extends Stmt
  case class StmtGoto(expr: Expr) extends Stmt
  case class StmtReturn(comb: Boolean, exprOpt: Option[Expr]) extends Stmt
  case class StmtAssign(lhs: Expr, rhs: Expr) extends Stmt
  case class StmtUpdate(lhs: Expr, op: String, rhs: Expr) extends Stmt
  case class StmtPost(expr: Expr, op: String) extends Stmt
  case class StmtDelayed(lhs: Expr, rhs: Expr) extends Stmt
  case class StmtOutcall(output: Expr, func: Expr, inputs: List[Expr]) extends Stmt
  case class StmtRead() extends Stmt
  case class StmtWrite() extends Stmt
  case class StmtExpr(expr: Expr) extends Stmt
  case class StmtStall(cond: Expr) extends Stmt
  case class StmtAssertion(assertion: Assertion) extends Stmt
  case class StmtError() extends Stmt
  case class StmtComment(str: String) extends Stmt
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Case clauses
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Case extends Tree with CaseOps

  case class CaseGen(gen: Gen) extends Case
  case class CaseRegular(cond: List[Expr], override val stmts: List[Stmt]) extends Case
  case class CaseDefault(override val stmts: List[Stmt]) extends Case

  //////////////////////////////////////////////////////////////////////////////
  // Expressions
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Expr extends Tree with ExprOps

  object Expr extends ExprObjOps

  case class ExprCall(expr: Expr, args: List[Arg]) extends Expr
  case class ExprUnary(op: String, expr: Expr) extends Expr
  case class ExprBinary(lhs: Expr, op: String, rhs: Expr) extends Expr
  case class ExprTernary(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class ExprRep(count: Expr, expr: Expr) extends Expr
  case class ExprCat(parts: List[Expr]) extends Expr
  case class ExprIndex(expr: Expr, index: Expr) extends Expr
  case class ExprSlice(expr: Expr, lIdx: Expr, op: String, rIdx: Expr) extends Expr
  case class ExprSelect(expr: Expr, selector: String, idxs: List[Expr]) extends Expr
  case class ExprRef(ref: Ref) extends Expr
  case class ExprSym(symbol: Symbol) extends Expr
  case class ExprThis(expr: Expr) extends Expr
  case class ExprType(kind: TypeFund) extends Expr
  case class ExprCast(kind: TypeFund, expr: Expr) extends Expr
  case class ExprInt(signed: Boolean, width: Int, value: BigInt) extends Expr with ExprIntImpl
  case class ExprNum(signed: Boolean, value: BigInt) extends Expr with ExprNumImpl
  case class ExprStr(value: String) extends Expr
  case class ExprError() extends Expr
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Argument assignments
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Arg extends Tree with ArgOps

  case class ArgP(override val expr: Expr) extends Arg
  case class ArgN(name: String, override val expr: Expr) extends Arg

  //////////////////////////////////////////////////////////////////////////////
  // Thicket/Stump
  //////////////////////////////////////////////////////////////////////////////

  // Thicket is used where a node needs to be transformed into a list of nodes.
  // Thickets are transient and are flattened into the receiving list during
  // traversal. Any node type T that can be transformed into a Thicket must
  // always be held via a List[T] by parent nodes, or via a node which only has
  // a single child, is itself held by a List[T], and is walked in the
  // TreeTransformer via the 'splice' method. Nodes with his property that are
  // therefore valid to be re-written into a Thicket are:
  //   Desc, Decl, Defn, Stmt, Ent, Rec, Riz
  case class Thicket(trees: List[Tree]) extends Tree {
    this withLoc Loc.synthetic
    this withTpe TypeMisc
  }

  // Stump is the same as an empty thicket, meaning remove this node
  case object Stump extends Tree {
    this withLoc Loc.synthetic
    this withTpe TypeMisc
  }

}

trait TreeImpl extends Product with Locationed { this: Trees.Tree =>

  // A Tree can have children held either directly, via an Iterable or via an Option
  def children: Iterator[Trees.Tree] = productIterator flatMap {
    case tree: Trees.Tree       => Iterator.single(tree)
    case trees: Iterable[_]     => trees.iterator collect { case tree: Trees.Tree => tree }
    case Some(tree: Trees.Tree) => Iterator.single(tree)
    case _                      => Iterator.empty
  }

}

trait IdentImpl { this: Trees.Ident =>
  val attr: mutable.Map[String, SourceAttribute] = mutable.Map()
}

trait DescImpl { this: Trees.Desc =>
  ref match {
    case Trees.Sym(symbol, _) => symbol.desc = this
    case _                    =>
  }
}

trait DeclImpl { this: Trees.Decl =>
  symbol.decl = this
}

trait DefnImpl { this: Trees.Defn =>
  symbol.defn = this
}

trait ExprIntImpl { this: Trees.ExprInt =>
  require(width > 0, s"width=$width")
  require(
    if (signed) {
      val lim = BigInt(1) << (width - 1)
      -lim <= value && value < lim
    } else {
      0 <= value && value < (BigInt(1) << width)
    },
    s"signed=$signed, value=$value"
  )
}

trait ExprNumImpl {
  this: Trees.ExprNum =>
  require(signed || value >= 0, s"signed=$signed, value=$value")
}
