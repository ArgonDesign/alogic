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
// The TypeAssigner computes and assigns types of nodes based on their children
// note that the TypeAssigner assumes that the node is correctly typed
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.CompoundType
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.util.chaining._

object TypeAssigner {
  //////////////////////////////////////////////////////////////////////////////
  // 'kind' methods compute the new type, assuming there were no type errors
  // in the child nodes
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Tree
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Tree)(implicit cc: CompilerContext): Type = tree match {
    case node: Root      => kind(node)
    case node: Ref       => kind(node)
    case node: Desc      => kind(node)
    case node: Decl      => kind(node)
    case node: Defn      => kind(node)
    case _: Gen          => unreachable
    case node: Assertion => kind(node)
    case node: Riz       => kind(node)
    case node: Ent       => kind(node)
    case node: Rec       => kind(node)
    case node: Stmt      => kind(node)
    case node: Case      => kind(node)
    case node: Expr      => kind(node)
    case node: Arg       => kind(node)
    case node: Thicket   => kind(node)
    case Stump           => kind(Stump)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Root
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Root) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Ref
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Ref) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Desc
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Desc) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Decl
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Decl) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Defn
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Defn) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Assertion
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Assertion) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Riz
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Riz): Type = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Ent
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Ent): Type = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Rec
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Rec): Type = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Stmt
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Stmt): Type = tree match {
    case node: StmtDesc      => kind(node)
    case node: StmtDecl      => kind(node)
    case node: StmtDefn      => kind(node)
    case _: StmtGen          => unreachable
    case node: StmtBlock     => kind(node)
    case node: StmtIf        => kind(node)
    case node: StmtCase      => kind(node)
    case node: StmtLoop      => kind(node)
    case node: StmtWhile     => kind(node)
    case node: StmtFor       => kind(node)
    case node: StmtDo        => kind(node)
    case node: StmtLet       => kind(node)
    case node: StmtFence     => kind(node)
    case node: StmtBreak     => kind(node)
    case node: StmtContinue  => kind(node)
    case node: StmtGoto      => kind(node)
    case node: StmtReturn    => kind(node)
    case node: StmtAssign    => kind(node)
    case node: StmtUpdate    => kind(node)
    case node: StmtPost      => kind(node)
    case node: StmtDelayed   => kind(node)
    case node: StmtOutcall   => kind(node)
    case node: StmtRead      => kind(node)
    case node: StmtWrite     => kind(node)
    case node: StmtExpr      => kind(node)
    case node: StmtStall     => kind(node)
    case node: StmtAssertion => kind(node)
    case node: StmtError     => kind(node)
    case node: StmtComment   => kind(node)
  }

  private def kind(tree: StmtDesc) = TypeCombStmt

  private def kind(tree: StmtDecl) = TypeCombStmt

  private def kind(tree: StmtDefn) = TypeCombStmt

  private def kind(tree: StmtBlock) = tree.body match {
    case Nil => TypeCombStmt
    case ss  => ss.last.tpe
  }

  private def kind(tree: StmtIf) = tree match {
    case StmtIf(_, Nil, Nil) => TypeCombStmt
    case StmtIf(_, ts, Nil)  => ts.last.tpe
    case StmtIf(_, _, es)    => es.last.tpe
  }

  private def kind(tree: StmtCase) = tree.cases match {
    case Nil => TypeCombStmt
    case kase :: _ =>
      kase.stmts match {
        case Nil   => TypeCombStmt
        case stmts => stmts.last.tpe
      }
  }

  private def kind(tree: StmtLoop) = TypeCtrlStmt

  private def kind(tree: StmtWhile) = TypeCtrlStmt

  private def kind(tree: StmtFor) = TypeCtrlStmt

  private def kind(tree: StmtDo) = TypeCtrlStmt

  private def kind(tree: StmtLet) = tree.body match {
    case Nil => TypeCombStmt
    case ss  => ss.last.tpe
  }

  private def kind(tree: StmtFence) = TypeCtrlStmt

  private def kind(tree: StmtBreak) = TypeCtrlStmt

  private def kind(tree: StmtContinue) = TypeCtrlStmt

  private def kind(tree: StmtGoto) = TypeCtrlStmt

  private def kind(tree: StmtReturn) = if (tree.comb) TypeCombStmt else TypeCtrlStmt

  private def kind(tree: StmtAssign) = TypeCombStmt

  private def kind(tree: StmtUpdate) = TypeCombStmt

  private def kind(tree: StmtPost) = TypeCombStmt

  private def kind(tree: StmtDelayed) = TypeCombStmt

  private def kind(tree: StmtOutcall) = TypeCombStmt

  private def kind(tree: StmtRead) = TypeCombStmt

  private def kind(tree: StmtWrite) = TypeCombStmt

  private def kind(tree: StmtExpr) = tree.expr match {
    case ExprCall(target, _) if target.tpe.isCtrlFunc => TypeCtrlStmt
    case _                                            => TypeCombStmt
  }

  private def kind(tree: StmtStall) = TypeCombStmt

  private def kind(tree: StmtAssertion) = TypeCombStmt

  private def kind(tree: StmtError) = TypeError

  private def kind(tree: StmtComment) = TypeCombStmt

  //////////////////////////////////////////////////////////////////////////////
  // Case
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Case) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Expr
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Expr)(implicit cc: CompilerContext): Type = tree match {
    case node: ExprCall    => kind(node)
    case node: ExprUnary   => kind(node)
    case node: ExprBinary  => kind(node)
    case node: ExprTernary => kind(node)
    case node: ExprRep     => kind(node)
    case node: ExprCat     => kind(node)
    case node: ExprIndex   => kind(node)
    case node: ExprSlice   => kind(node)
    case node: ExprSelect  => kind(node)
    case _: ExprRef        => unreachable
    case node: ExprSym     => kind(node)
    case node: ExprThis    => kind(node)
    case node: ExprType    => kind(node)
    case node: ExprCast    => kind(node)
    case node: ExprInt     => kind(node)
    case node: ExprNum     => kind(node)
    case node: ExprStr     => kind(node)
    case node: ExprError   => kind(node)
  }

  private def kind(tree: ExprCall)(implicit cc: CompilerContext) = tree.expr.tpe match {
    case TypeCombFunc(_, returnType, _)     => returnType
    case TypeCtrlFunc(_, returnType, _)     => returnType
    case TypePolyFunc(_, resolver)          => resolver(tree.args).get.kind.asCombFunc.retType
    case TypeXenoFunc(_, returnType, _)     => returnType
    case TypeStaticMethod(_, returnType, _) => returnType
    case TypeNormalMethod(_, returnType, _) => returnType
    case _: TypeType                        => TypeUnknown
    case _: TypeParametrized                => TypeUnknown
    case _                                  => unreachable
  }

  private def kind(tree: ExprUnary) = tree.op match {
    case "'"             => unreachable
    case "+" | "-" | "~" => tree.expr.tpe
    case _               => TypeUInt(1)
  }

  private def kind(tree: ExprBinary)(implicit cc: CompilerContext) = tree.op match {
    case ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||" => TypeUInt(1)
    case "<<" | ">>" | "<<<" | ">>>"                         => tree.lhs.tpe
    case _ =>
      val lTpe = tree.lhs.tpe
      val rTpe = tree.rhs.tpe
      val signed = lTpe.isSigned && rTpe.isSigned
      if (lTpe.underlying.isNum && rTpe.underlying.isNum) {
        TypeNum(signed)
      } else {
        val width = if (lTpe.underlying.isNum) rTpe.width else lTpe.width
        TypeInt(signed, width)
      }
  }

  private def kind(tree: ExprTernary)(implicit cc: CompilerContext) = {
    val tTpe = tree.thenExpr.tpe
    val eTpe = tree.elseExpr.tpe
    val signed = tTpe.isSigned && eTpe.isSigned
    if (tTpe.underlying.isNum && eTpe.underlying.isNum) {
      TypeNum(signed)
    } else {
      val width = if (tTpe.underlying.isNum) eTpe.width else tTpe.width
      TypeInt(signed, width)
    }
  }

  private def kind(tree: ExprRep)(implicit cc: CompilerContext) = {
    TypeUInt(tree.count.value.get * tree.expr.tpe.width)
  }

  private def kind(tree: ExprCat)(implicit cc: CompilerContext) = {
    TypeUInt(tree.parts.foldLeft(BigInt(0))(_ + _.tpe.width))
  }

  private def kind(tree: ExprIndex)(implicit cc: CompilerContext) = {
    tree.expr.tpe.underlying match {
      case _: TypeNum          => TypeUInt(1)
      case _: TypeInt          => TypeUInt(1)
      case TypeArray(kind, _)  => kind
      case TypeVector(kind, _) => kind
      case TypeType(kind)      => TypeType(kind addVectorDimension tree.index.value.get)
      case _                   => unreachable
    }
  }

  private def kind(tree: ExprSlice)(implicit cc: CompilerContext) = {
    val size = if (tree.op == ":") {
      tree.lIdx.value.get - tree.rIdx.value.get + 1
    } else {
      tree.rIdx.value.get
    }
    tree.expr.tpe.underlying match {
      case _: TypeNum          => TypeUInt(size)
      case _: TypeInt          => TypeUInt(size)
      case TypeVector(kind, _) => TypeVector(kind, size)
      case _                   => unreachable
    }
  }

  private def kind(tree: ExprSelect)(implicit cc: CompilerContext) =
    tree.expr.tpe pipe {
      case TypeType(kind: CompoundType) =>
        // TODO: Relax select on TypeType if the result is a proper type (i.e.:
        // TypeType(k) such that k can be instantiated without an instance of
        // the enclosing type)
        kind(tree.selector).get.kind match {
          // Static function is accessible via type
          case k: TypeStaticMethod => k
          // Other things are not
          case TypeType(k) => TypeNone(k)
          case k           => TypeNone(k)
        }
      case TypeNone(kind: CompoundType) => TypeNone(kind(tree.selector).get.kind)
      case kind: CompoundType           => kind(tree.selector).get.kind
      case _                            => unreachable
    }

  private def kind(tree: ExprSym)(implicit cc: CompilerContext) = tree.symbol.kind match {
    // If this is a reference to a choice symbol. Then we don't know it's type yet.
    case TypeChoice => TypeUnknown
    // Parameters should have been specialized by type checking
    case _: TypeParam => unreachable
    // TODO: lose these
    case TypeConst(kind)    => kind
    case TypePipeline(kind) => kind
    //
    case other => other
  }

  private def kind(tree: ExprThis) = tree.expr.tpe.asType.kind

  private def kind(tree: ExprType) = TypeType(tree.kind)

  private def kind(node: ExprCast) = node.kind

  private def kind(tree: ExprInt) = TypeInt(tree.signed, tree.width)

  private def kind(tree: ExprNum) = TypeNum(tree.signed)

  private def kind(tree: ExprStr) = TypeStr

  private def kind(tree: ExprError) = TypeError

  //////////////////////////////////////////////////////////////////////////////
  // Arg
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Arg) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Thicket/Stump // TODO: should be unreachable
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Thicket) = TypeMisc

  private def kind(tree: Stump.type) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // 'apply' methods propagate type errors or assign the computed type. There
  // are lots of overloads of these to use static dispatch wherever possible,
  // but otherwise they are exactly the same. There should be one for each
  // 'kind' method above.
  //////////////////////////////////////////////////////////////////////////////

  // format: off
  def apply(tree: Tree)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: Root): tree.type = assign(tree)(kind(tree))
  def apply(tree: Ref): tree.type = assign(tree)(kind(tree))
  def apply(tree: Desc): tree.type = assign(tree)(kind(tree))
  def apply(tree: Decl): tree.type = assign(tree)(kind(tree))
  def apply(tree: Defn): tree.type = assign(tree)(kind(tree))
  def apply(tree: Assertion): tree.type = assign(tree)(kind(tree))
  def apply(tree: Riz): tree.type = assign(tree)(kind(tree))
  def apply(tree: Ent): tree.type = assign(tree)(kind(tree))
  def apply(tree: Rec): tree.type = assign(tree)(kind(tree))
  def apply(tree: Stmt): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtBlock): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtIf): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtCase): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtExpr): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtLoop): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtWhile): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtFor): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtDo): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtLet): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtFence): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtBreak): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtContinue): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtGoto): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtReturn): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtAssign): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtUpdate): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtPost): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtDelayed): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtOutcall): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtDesc): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtDecl): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtDefn): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtRead): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtWrite): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtComment): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtStall): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtAssertion): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtError): tree.type = assign(tree)(kind(tree))
  def apply(tree: Case): tree.type = assign(tree)(kind(tree))
  def apply(tree: Expr)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCall)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprUnary): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprBinary)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprTernary)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprRep)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCat)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprIndex)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSlice)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSelect)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSym)(implicit cc: CompilerContext): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprThis): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprType): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCast): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprInt): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprNum): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprStr): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprError): tree.type = assign(tree)(kind(tree))
  def apply(tree: Arg): tree.type = assign(tree)(kind(tree))
  def apply(tree: Thicket): tree.type = assign(tree)(kind(tree))
  // format: on

  private def assign(tree: Tree)(kind: => Type): tree.type = {
    require(!tree.hasTpe, tree.tpe)
    def hasError(node: Tree): Boolean = node.children exists {
      case child: EntConnect if !child.hasTpe => false // TODO: remove
      case child: Tree if !child.hasTpe       => unreachable // TODO: remove
      case child: Tree                        => child.tpe.isError
    }
    val tpe = if (hasError(tree)) TypeError else kind
    if (!tree.hasTpe) {
      tree withTpe tpe
    }
    tree
  }

}
