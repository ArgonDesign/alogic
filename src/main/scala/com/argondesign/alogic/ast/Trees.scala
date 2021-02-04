////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// AST representation used throughout the compiler
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.builtins.Builtin
import com.argondesign.alogic.core.FlowControlTypes.FlowControlType
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Locationed
import com.argondesign.alogic.core.StorageTypes.StorageType
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.SymbolTable

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future

object Trees {

  private val nextId = new AtomicInteger(0)

  //////////////////////////////////////////////////////////////////////////////
  // AST base type
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Tree extends TreeImpl with TreeOps {
    val id: Int = nextId.getAndIncrement()
  }

  object Tree extends ObjectTreeOps

  //////////////////////////////////////////////////////////////////////////////
  // Marker trait for generic trees that can be spliced into more specific trees
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Spliceable extends Tree

  //////////////////////////////////////////////////////////////////////////////
  // Base trait for simple nodes which splice generic trees in to specific trees
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Splice extends Tree { val tree: Spliceable }
  object Splice extends SpliceObjOps

  //////////////////////////////////////////////////////////////////////////////
  // Ref (reference) nodes refer to names/symbols prior to elaboration
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Ref extends Tree

  case class Ident(base: String, idxs: List[Expr]) extends Ref
  case class Sym(symbol: Symbol) extends Ref

  //////////////////////////////////////////////////////////////////////////////
  // Desc (description) nodes introduce names (symbols) as in source code,
  // they only exist in the frontend, after which they are split into pairs of
  // Decl/Defn nodes
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Desc extends Tree with Spliceable with DescImpl with DescOps {
    val ref: Ref
    val attr: List[Attr]
  }

  object Desc extends DescObjOps

  case class DescVar(ref: Ref, attr: List[Attr], spec: Expr, initOpt: Option[Expr]) extends Desc
  case class DescVal(ref: Ref, attr: List[Attr], spec: Expr, init: Expr) extends Desc
  case class DescStatic(ref: Ref, attr: List[Attr], spec: Expr, initOpt: Option[Expr]) extends Desc
  case class DescIn(ref: Ref, attr: List[Attr], spec: Expr, fc: FlowControlType) extends Desc
  case class DescOut(ref: Ref, attr: List[Attr], spec: Expr, fc: FlowControlType, st: StorageType, initOpt: Option[Expr]) extends Desc
  case class DescPipeVar(ref: Ref, attr: List[Attr], spec: Expr) extends Desc
  case class DescPipeIn(ref: Ref, attr: List[Attr], fc: FlowControlType) extends Desc
  case class DescPipeOut(ref: Ref, attr: List[Attr], fc: FlowControlType, st: StorageType) extends Desc
  case class DescParam(ref: Ref, attr: List[Attr], spec: Expr, initOpt: Option[Expr], finished: Boolean) extends Desc
  case class DescParamType(ref: Ref, attr: List[Attr], initOpt: Option[Expr], finished: Boolean) extends Desc
  case class DescConst(ref: Ref, attr: List[Attr], spec: Expr, init: Expr) extends Desc
  case class DescArray(ref: Ref, attr: List[Attr], elem: Expr, size: Expr) extends Desc
  case class DescSram(ref: Ref, attr: List[Attr], elem: Expr, size: Expr, st: StorageType) extends Desc
  case class DescType(ref: Ref, attr: List[Attr], spec: Expr) extends Desc
  case class DescEntity(ref: Ref, attr: List[Attr], variant: EntityVariant.Type, body: List[Ent]) extends Desc
  case class DescRecord(ref: Ref, attr: List[Attr], body: List[Rec]) extends Desc 
  case class DescInstance(ref: Ref, attr: List[Attr], spec: Expr) extends Desc
  case class DescSingleton(ref: Ref, attr: List[Attr], variant: EntityVariant.Type, body: List[Ent]) extends Desc
  case class DescFunc(ref: Ref, attr: List[Attr], variant: FuncVariant, ret: Expr, args: List[Desc], body: List[Stmt]) extends Desc
  case class DescPackage(ref: Ref, attr: List[Attr], body: List[Pkg]) extends Desc 
  case class DescGenVar(ref: Ref, attr: List[Attr], spec: Expr, init: Expr) extends Desc
  case class DescGenIf(ref: Ref, attr: List[Attr], cases: List[GenCase], defaults: List[Tree]) extends Desc
  case class DescGenFor(ref: Ref, attr: List[Attr], inits: List[Desc], cond: Expr, steps: List[Stmt], body: List[Tree]) extends Desc
  case class DescGenRange(ref: Ref, attr: List[Attr], init: Desc, op: String, end: Expr, body: List[Tree]) extends Desc
  case class DescGenScope(ref: Ref, attr: List[Attr], body: List[Tree]) extends Desc
  case class DescAlias(ref: Ref, attr: List[Attr], expr: Expr, exprt: Boolean) extends Desc
  case class DescParametrized(ref: Ref, attr: List[Attr], desc: Desc, symtab: SymbolTable) extends Desc
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Attributes (attached to names/symbols)
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Attr extends Tree { val name: String }

  object Attr extends AttrObjOps

  case class AttrBool(name: String) extends Attr
  case class AttrExpr(name: String, expr: Expr) extends Attr

  //////////////////////////////////////////////////////////////////////////////
  // Case clause for 'gen' conditionals
  //////////////////////////////////////////////////////////////////////////////

  case class GenCase(cond: Expr, body: List[Tree]) extends Tree

  //////////////////////////////////////////////////////////////////////////////
  // Decl (declaration) nodes specify the type of symbols
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Decl extends Tree with Spliceable with DeclImpl with DeclOps { val symbol: Symbol }

  object Decl extends DeclObjOps

  case class DeclVar(symbol: Symbol, spec: Expr) extends Decl
  case class DeclVal(symbol: Symbol, spec: Expr) extends Decl
  case class DeclStatic(symbol: Symbol, spec: Expr) extends Decl
  case class DeclIn(symbol: Symbol, spec: Expr, fc: FlowControlType) extends Decl
  case class DeclOut(symbol: Symbol, spec: Expr, fc: FlowControlType, st: StorageType) extends Decl
  case class DeclPipeVar(symbol: Symbol, spec: Expr) extends Decl
  case class DeclPipeIn(symbol: Symbol, fc: FlowControlType) extends Decl
  case class DeclPipeOut(symbol: Symbol, fc: FlowControlType, st: StorageType) extends Decl
  case class DeclConst(symbol: Symbol, spec: Expr) extends Decl
  case class DeclArray(symbol: Symbol, elem: Expr, size: Long) extends Decl
  case class DeclSram(symbol: Symbol, elem: Expr, size: Long, st: StorageType) extends Decl
  case class DeclStack(symbol: Symbol, elem: Expr, size: Long) extends Decl
  case class DeclType(symbol: Symbol, spec: Expr) extends Decl
  case class DeclEntity(symbol: Symbol, decls: List[Decl]) extends Decl with DeclEntityOps
  case class DeclRecord(symbol: Symbol,  decls: List[Decl]) extends Decl 
  case class DeclInstance(symbol: Symbol, spec: Expr) extends Decl
  case class DeclSingleton(symbol: Symbol, decls: List[Decl]) extends Decl with DeclSingletonOps
  case class DeclFunc(symbol: Symbol, variant: FuncVariant, ret: Expr, args: List[Decl]) extends Decl
  case class DeclState(symbol: Symbol) extends Decl
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Defn (definition) nodes specify the implementation of symbols
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Defn extends Tree with Spliceable with DefnImpl with DefnOps { val symbol: Symbol }

  object Defn extends DefnObjOps

  case class DefnVar(symbol: Symbol, initOpt: Option[Expr]) extends Defn
  case class DefnVal(symbol: Symbol, init: Expr) extends Defn
  case class DefnStatic(symbol: Symbol, initOpt: Option[Expr]) extends Defn
  case class DefnIn(symbol: Symbol) extends Defn
  case class DefnOut(symbol: Symbol, initOpt: Option[Expr]) extends Defn
  case class DefnPipeVar(symbol: Symbol) extends Defn
  case class DefnPipeIn(symbol: Symbol) extends Defn
  case class DefnPipeOut(symbol: Symbol) extends Defn
  case class DefnConst(symbol: Symbol, init: Expr) extends Defn
  case class DefnArray(symbol: Symbol) extends Defn
  case class DefnSram(symbol: Symbol) extends Defn
  case class DefnStack(symbol: Symbol) extends Defn
  case class DefnType(symbol: Symbol) extends Defn
  case class DefnEntity(symbol: Symbol, variant: EntityVariant.Type, body: List[Ent]) extends Defn with DefnEntityOps
  case class DefnRecord(symbol: Symbol, body: List[Rec]) extends Defn with DefnRecordOps
  case class DefnInstance(symbol: Symbol) extends Defn
  case class DefnSingleton(symbol: Symbol, variant: EntityVariant.Type, body: List[Ent]) extends Defn 
  case class DefnFunc(symbol: Symbol, args: List[Defn], body: List[Stmt]) extends Defn 
  case class DefnState(symbol: Symbol, body: List[Stmt]) extends Defn
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Import
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Import extends Tree with Spliceable

  case class ImportOne(path: String, ident: Ident) extends Import
  case class ImportPending(future: Future[FinalResult[Symbol]], ident: Ident) extends Import

  //////////////////////////////////////////////////////////////////////////////
  // Using
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Using extends Tree with Spliceable with UsingOps { val expr: Expr }

  case class UsingOne(expr: Expr, identOpt: Option[Ident]) extends Using
  case class UsingAll(expr: Expr, exprt: Boolean) extends Using
  case class UsingGenLoopBody(expr: Expr, exclude: Set[Symbol]) extends Using

  //////////////////////////////////////////////////////////////////////////////
  // From
  //////////////////////////////////////////////////////////////////////////////

  sealed trait From extends Tree with Spliceable

  case class FromOne(path: String, name: Expr, identOpt: Option[Ident]) extends From
  case class FromAll(path: String) extends From

  //////////////////////////////////////////////////////////////////////////////
  // Assertion
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Assertion extends Tree with Spliceable

  case class AssertionAssert(cond: Expr, msgOpt: Option[String]) extends Assertion
  case class AssertionAssume(cond: Expr, msgOpt: Option[String]) extends Assertion
  case class AssertionStatic(cond: Expr, msgOpt: Option[String]) extends Assertion
  case class AssertionUnreachable(comb: Boolean, msgOpt: Option[String]) extends Assertion

  //////////////////////////////////////////////////////////////////////////////
  // Package (file) contents
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Pkg extends Tree

  case class PkgSplice(tree: Spliceable) extends Pkg with Splice
  case class PkgCompile(expr: Expr, identOpt: Option[Ident]) extends Pkg

  //////////////////////////////////////////////////////////////////////////////
  // Entity contents
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Ent extends Tree

  case class EntSplice(tree: Spliceable) extends Ent with Splice
  case class EntConnect(lhs: Expr, rhs: List[Expr]) extends Ent
  case class EntAssign(lhs: Expr, rhs: Expr) extends Ent
  case class EntCombProcess(stmts: List[Stmt]) extends Ent
  case class EntClockedProcess(clk: Expr, rstOpt: Option[Expr], stmts: List[Stmt]) extends Ent
  case class EntVerbatim(lang: String, body: String) extends Ent
  case class EntComment(str: String) extends Ent

  //////////////////////////////////////////////////////////////////////////////
  // Record contents
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Rec extends Tree

  case class RecSplice(tree: Spliceable) extends Rec with Splice
  case class RecComment(str: String) extends Rec

  //////////////////////////////////////////////////////////////////////////////
  // Statements
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Stmt extends Tree with StmtOps

  case class StmtSplice(tree: Spliceable) extends Stmt with Splice
  case class StmtBlock(body: List[Stmt]) extends Stmt
  case class StmtIf(cond: Expr, thenStmts: List[Stmt], elseStmts: List[Stmt]) extends Stmt
  case class StmtCase(expr: Expr, cases: List[Case]) extends Stmt with StmtCaseOps
  case class StmtLoop(body: List[Stmt]) extends Stmt
  case class StmtWhile(cond: Expr, body: List[Stmt]) extends Stmt
  case class StmtFor(inits: List[Stmt], condOpt: Option[Expr], steps: List[Stmt], body: List[Stmt]) extends Stmt
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
  case class StmtExpr(expr: Expr) extends Stmt
  case class StmtWait(cond: Expr) extends Stmt
  case class StmtComment(str: String) extends Stmt
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Case clauses - in StmtCase
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Case extends Tree with CaseOps

  case class CaseSplice(tree: Spliceable) extends Case with Splice // Only to support 'gen'
  case class CaseRegular(cond: List[Expr], override val stmts: List[Stmt]) extends Case
  case class CaseDefault(override val stmts: List[Stmt]) extends Case

  //////////////////////////////////////////////////////////////////////////////
  // Expressions
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  sealed trait Expr extends Tree with ExprOps

  object Expr extends ExprObjOps

  case class ExprCall(expr: Expr, args: List[Arg]) extends Expr
  case class ExprBuiltin(builtin: Builtin, args: List[Arg]) extends Expr
  case class ExprUnary(op: String, expr: Expr) extends Expr
  case class ExprBinary(lhs: Expr, op: String, rhs: Expr) extends Expr
  case class ExprCond(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class ExprRep(count: Expr, expr: Expr) extends Expr
  case class ExprCat(parts: List[Expr]) extends Expr
  case class ExprIndex(expr: Expr, index: Expr) extends Expr
  case class ExprSlice(expr: Expr, lIdx: Expr, op: String, rIdx: Expr) extends Expr
  case class ExprDot(expr: Expr, selector: String, idxs: List[Expr]) extends Expr
  case class ExprSymSel(expr: Expr, symbol: Symbol) extends Expr
  case class ExprSel(expr: Expr, selector: String) extends Expr
  case class ExprIdent(base: String, idxs: List[Expr]) extends Expr
  case class ExprSym(symbol: Symbol) extends Expr
  case class ExprOld(expr: Expr) extends Expr
  case class ExprThis(expr: Expr) extends Expr
  case class ExprType(kind: TypeFund) extends Expr
  case class ExprCast(kind: TypeFund, expr: Expr) extends Expr
  case class ExprInt(signed: Boolean, width: Int, override val value: BigInt) extends Expr with ExprIntImpl
  case class ExprNum(signed: Boolean,  override val value: BigInt) extends Expr with ExprNumImpl
  case class ExprStr(v: String) extends Expr
  // format: on

  //////////////////////////////////////////////////////////////////////////////
  // Argument assignments
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Arg extends Tree { val expr: Expr }

  case class ArgP(override val expr: Expr) extends Arg
  case class ArgN(name: String, override val expr: Expr) extends Arg
  case class ArgD(base: String, idxs: List[Expr], override val expr: Expr) extends Arg

  //////////////////////////////////////////////////////////////////////////////
  // Thicket/Stump
  //////////////////////////////////////////////////////////////////////////////

  // Thicket is used where a node needs to be transformed into a list of nodes.
  // Thickets are transient and are flattened into the receiving list during
  // traversal. Any node type T that can be transformed into a Thicket must
  // always be held via a List[T] by parent nodes, or via a node which only has
  // a single child, is itself held by a List[T], and is walked in the
  // TreeTransformer via the 'splice' method. Nodes with this property that are
  // therefore valid to be re-written into a Thicket are:
  //   Desc, Decl, Defn, Pkg, Ent, Rec, Stmt
  case class Thicket(trees: List[Tree]) extends Tree {
    this withLoc Loc.synthetic
    this withTpe TypeMisc
  }

  object Thicket {
    def apply(trees: IterableOnce[Tree]): Thicket = Thicket(List.from(trees.iterator))
    def apply(trees: Tree*): Thicket = Thicket(List.from(trees.iterator))
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

trait DescImpl { this: Trees.Desc =>
  ref match {
    case Trees.Sym(symbol) => symbol.desc = this
    case _                 =>
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
