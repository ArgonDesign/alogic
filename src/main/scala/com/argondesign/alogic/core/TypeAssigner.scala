////////////////////////////////////////////////////////////////////////////////                                                                                          >
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.                                                                                                              >
//                                                                                                                                                                        >
// This file is covered by the BSD (with attribution) license.                                                                                                            >
// See the LICENSE file for the precise wording of the license.                                                                                                           >
//                                                                                                                                                                        >
// DESCRIPTION:                                                                                                                                                           >
// The TypeAssigner computes and assigns types of nodes based on their children                                                                                           >
// note that the TypeAssigner assumes that the node is correctly typed and
// has passed elaboration and therefore can be typed without knowing anything
// about the enclosing context or values of symbols.
////////////////////////////////////////////////////////////////////////////////                                                                                          >

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps

object TypeAssigner {
  //////////////////////////////////////////////////////////////////////////////
  // 'kind' methods compute the new type, assuming there were no type errors
  // in the child nodes
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Tree
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Tree): Type = tree match {
    case node: Ref       => kind(node)
    case node: Desc      => kind(node)
    case node: Attr      => kind(node)
    case _: GenCase      => throw Ice(tree, "TypeAssigner called on GenCase node")
    case node: Decl      => kind(node)
    case node: Defn      => kind(node)
    case _: Import       => throw Ice(tree, "TypeAssigner called on Import node")
    case _: Using        => throw Ice(tree, "TypeAssigner called on Using node")
    case _: From         => throw Ice(tree, "TypeAssigner called on From node")
    case node: Assertion => kind(node)
    case node: Pkg       => kind(node)
    case node: Ent       => kind(node)
    case node: Rec       => kind(node)
    case node: Stmt      => kind(node)
    case node: Case      => kind(node)
    case node: Expr      => kind(node)
    case node: Arg       => kind(node)
    case _: Thicket      => throw Ice(tree, "TypeAssigner called on Thicket")
    case Stump           => throw Ice(tree, "TypeAssigner called on Stump")
  }

  //////////////////////////////////////////////////////////////////////////////
  // Ref
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Ref) = tree match {
    case _: Ident => throw Ice(tree, "TypeAssigner called on Ident node")
    case _: Sym   => TypeMisc
  }

  //////////////////////////////////////////////////////////////////////////////
  // Desc
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Desc) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Attr
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Attr) = TypeMisc

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
  // Pkg
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Pkg): Type = TypeMisc

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
    case node: StmtSplice   => kind(node)
    case node: StmtBlock    => kind(node)
    case node: StmtIf       => kind(node)
    case node: StmtCase     => kind(node)
    case node: StmtLoop     => kind(node)
    case node: StmtWhile    => kind(node)
    case node: StmtFor      => kind(node)
    case node: StmtDo       => kind(node)
    case node: StmtLet      => kind(node)
    case node: StmtFence    => kind(node)
    case node: StmtBreak    => kind(node)
    case node: StmtContinue => kind(node)
    case node: StmtGoto     => kind(node)
    case node: StmtReturn   => kind(node)
    case node: StmtAssign   => kind(node)
    case node: StmtUpdate   => kind(node)
    case node: StmtPost     => kind(node)
    case node: StmtDelayed  => kind(node)
    case node: StmtOutcall  => kind(node)
    case node: StmtExpr     => kind(node)
    case node: StmtWait     => kind(node)
    case node: StmtError    => kind(node)
    case node: StmtComment  => kind(node)
  }

  private def kind(tree: StmtSplice) = tree.tree match {
    case DescGenScope(_, _, Nil)  => TypeCombStmt
    case DescGenScope(_, _, body) => body.last.tpe
    case _                        => TypeCombStmt
  }

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

  private def kind(tree: StmtExpr) = tree.expr match {
    case ExprCall(target, _) if target.tpe.isCtrlFunc => TypeCtrlStmt
    case _                                            => TypeCombStmt
  }

  private def kind(tree: StmtWait) = TypeCombStmt

  private def kind(tree: StmtError) = TypeError

  private def kind(tree: StmtComment) = TypeCombStmt

  //////////////////////////////////////////////////////////////////////////////
  // Case
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Case) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Expr
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Expr): Type = tree match {
    case node: ExprCall    => kind(node)
    case node: ExprBuiltin => kind(node)
    case node: ExprUnary   => kind(node)
    case node: ExprBinary  => kind(node)
    case node: ExprCond    => kind(node)
    case node: ExprRep     => kind(node)
    case node: ExprCat     => kind(node)
    case node: ExprIndex   => kind(node)
    case node: ExprSlice   => kind(node)
    case node: ExprDot     => kind(node)
    case node: ExprSel     => kind(node)
    case node: ExprSymSel  => kind(node)
    case _: ExprIdent      => unreachable
    case node: ExprSym     => kind(node)
    case node: ExprOld     => kind(node)
    case node: ExprThis    => kind(node)
    case node: ExprType    => kind(node)
    case node: ExprCast    => kind(node)
    case node: ExprInt     => kind(node)
    case node: ExprNum     => kind(node)
    case node: ExprStr     => kind(node)
    case node: ExprError   => kind(node)
  }

  private def kind(tree: ExprCall) = tree.expr.tpe match {
    case TypeCombFunc(_, returnType, _)     => returnType
    case TypeCtrlFunc(_, returnType, _)     => returnType
    case TypeXenoFunc(_, returnType, _)     => returnType
    case TypeStaticMethod(_, returnType, _) => returnType
    case TypeNormalMethod(_, returnType, _) => returnType
    case _                                  => unreachable
  }

  private def kind(tree: ExprBuiltin) = tree.builtin.returnType(tree.args)

  private def kind(tree: ExprUnary) = tree.op match {
    case "'"             => unreachable
    case "+" | "-" | "~" => tree.expr.tpe
    case _               => TypeUInt(1)
  }

  private def kind(tree: ExprBinary) = tree.op match {
    case ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||" => TypeUInt(1)
    case "<<" | ">>" | "<<<" | ">>>"                         => tree.lhs.tpe
    case "'" =>
      TypeInt(tree.rhs.tpe.isSigned, tree.lhs.valueOption.get.asLong)
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

  private def kind(tree: ExprCond) = {
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

  private def kind(tree: ExprRep) = {
    TypeUInt(tree.count.valueOption.get.asLong * tree.expr.tpe.width)
  }

  private def kind(tree: ExprCat) = {
    TypeUInt(tree.parts.iterator.map(_.tpe.width).sum)
  }

  private def kind(tree: ExprIndex) = {
    tree.expr.tpe.underlying match {
      case _: TypeNum                              => TypeUInt(1)
      case TypeArray(kind, _)                      => kind
      case TypeVector(kind, _)                     => kind
      case TypeType(kind)                          => TypeType(kind addVectorDimension tree.index.valueOption.get.asLong)
      case kind if kind.isPacked && kind.width > 0 => TypeUInt(1)
      case _                                       => unreachable
    }
  }

  private def kind(tree: ExprSlice) = {
    val size = if (tree.op == ":") {
      tree.lIdx.valueOption.get - tree.rIdx.valueOption.get + 1
    } else {
      tree.rIdx.valueOption.get
    }
    tree.expr.tpe.underlying match {
      case _: TypeNum                              => TypeUInt(size.asLong)
      case TypeVector(kind, _)                     => TypeVector(kind, size.asLong)
      case kind if kind.isPacked && kind.width > 0 => TypeUInt(size.asLong)
      case _                                       => unreachable
    }
  }

  private def selKind(tgt: Expr, resKind: TypeCompound => Type): Type = tgt.tpe match {
    case TypeType(kind: TypeCompound) =>
      // TODO: Relax select on TypeType if the result is a proper type (i.e.:
      // TypeType(k) such that k can be instantiated without an instance of
      // the enclosing type)
      resKind(kind) match {
        // Static function is accessible via type
        case k: TypeStaticMethod => k
        // Other things are not
        case TypeType(k) => TypeNone(k)
        case k           => TypeNone(k)
      }
    case TypeNone(kind: TypeCompound) => TypeNone(resKind(kind))
    case kind: TypeCompound           => resKind(kind)
    case _                            => unreachable
  }

  private def kind(tree: ExprDot): Type = {
    require(tree.idxs.isEmpty)
    selKind(tree.expr, _.apply(tree.selector).get.kind)
  }

  private def kind(tree: ExprSel): Type =
    selKind(tree.expr, _.apply(tree.selector).get.kind)

  private def kind(tree: ExprSymSel): Type =
    selKind(tree.expr, _ => tree.symbol.kind)

  private def kind(tree: ExprSym) = tree.symbol.kind match {
    // Parameters should have been specialized prior to type checking
    case _: TypeParam => unreachable
    // TODO: lose these
    case TypeConst(kind)   => kind
    case TypePipeVar(kind) => kind
    //
    case other => other
  }

  private def kind(tree: ExprOld) = tree.expr.tpe

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
  // 'apply' methods propagate type errors or assign the computed type. There
  // are lots of overloads of these to use static dispatch wherever possible,
  // but otherwise they are exactly the same. There should be one for each
  // 'kind' method above.
  //////////////////////////////////////////////////////////////////////////////

  // $COVERAGE-OFF$ As mentioned above, these are just for static dispatch
  // format: off
  def apply(tree: Tree): tree.type = assign(tree)(kind(tree))
  def apply(tree: Desc): tree.type = assign(tree)(kind(tree))
  def apply(tree: Attr): tree.type = assign(tree)(kind(tree))
  def apply(tree: Decl): tree.type = assign(tree)(kind(tree))
  def apply(tree: Defn): tree.type = assign(tree)(kind(tree))
  def apply(tree: Assertion): tree.type = assign(tree)(kind(tree))
  def apply(tree: Pkg): tree.type = assign(tree)(kind(tree))
  def apply(tree: Ent): tree.type = assign(tree)(kind(tree))
  def apply(tree: Rec): tree.type = assign(tree)(kind(tree))
  def apply(tree: Stmt): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtSplice): tree.type = assign(tree)(kind(tree))
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
  def apply(tree: StmtComment): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtWait): tree.type = assign(tree)(kind(tree))
  def apply(tree: StmtError): tree.type = assign(tree)(kind(tree))
  def apply(tree: Case): tree.type = assign(tree)(kind(tree))
  def apply(tree: Expr): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCall): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprBuiltin): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprUnary): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprBinary): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCond): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprRep): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCat): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprIndex): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSlice): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprDot): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSel): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSymSel): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprSym): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprOld): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprThis): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprType): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprCast): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprInt): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprNum): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprStr): tree.type = assign(tree)(kind(tree))
  def apply(tree: ExprError): tree.type = assign(tree)(kind(tree))
  def apply(tree: Arg): tree.type = assign(tree)(kind(tree))
  // format: on
  // $COVERAGE-ON$

  private def assign(tree: Tree)(kind: => Type): tree.type = {
    require(!tree.hasTpe, tree.tpe)
    def hasError(node: Tree): Boolean = node.children exists {
      case child: EntConnect if !child.hasTpe => false // TODO: remove
      case child: Tree if !child.hasTpe       => println(child); unreachable // TODO: remove
      case child: Tree                        => child.tpe.isError
    }
    val tpe = if (!tree.isInstanceOf[DescParametrized] && hasError(tree)) TypeError else kind
    if (!tree.hasTpe) {
      tree withTpe tpe
    }
    tree
  }

}
