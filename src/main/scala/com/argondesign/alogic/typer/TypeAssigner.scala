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
import com.argondesign.alogic.core.Symbols.ErrorSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.util.unreachable

import scala.language.postfixOps

final object TypeAssigner {
  //////////////////////////////////////////////////////////////////////////////
  // 'kind' methods compute the new type, assuming there were no type errors
  // in the child nodes
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Tree)(implicit cc: CompilerContext): Type = tree match {
    case node: Expr           => kind(node)
    case node: Stmt           => kind(node)
    case node: RegularCase    => kind(node)
    case node: DefaultCase    => kind(node)
    case node: Entity         => kind(node)
    case node: Decl           => kind(node)
    case node: Instance       => kind(node)
    case node: Connect        => kind(node)
    case node: Function       => kind(node)
    case node: State          => kind(node)
    case node: Sym            => kind(node)
    case node: TypeDefinition => kind(node)
    case node: Thicket        => kind(node)
    case _: Root              => unreachable
    case _: DeclIdent         => unreachable
    case _: Ident             => unreachable
    case _: Gen               => unreachable
  }

  //////////////////////////////////////////////////////////////////////////////
  // Typing Misc nodes
  //////////////////////////////////////////////////////////////////////////////

  private def kind(node: Entity) = TypeMisc
  private def kind(node: Decl) = TypeMisc
  private def kind(node: Instance) = TypeMisc
  private def kind(node: Connect) = TypeMisc
  private def kind(node: Function) = TypeMisc
  private def kind(node: State) = TypeMisc
  private def kind(node: TypeDefinition) = TypeMisc
  private def kind(node: Thicket) = TypeMisc
  private def kind(node: RegularCase) = TypeMisc
  private def kind(node: DefaultCase) = TypeMisc

  //////////////////////////////////////////////////////////////////////////////
  // Typing Sym
  //////////////////////////////////////////////////////////////////////////////

  private def kind(node: Sym) = node.symbol.kind

  //////////////////////////////////////////////////////////////////////////////
  // Typing Stmt nodes
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Stmt): Type = tree match {
    case node: StmtBlock => kind(node)
    case node: StmtIf    => kind(node)
    case node: StmtCase  => kind(node)
    case node: StmtExpr  => kind(node)
    // Unambiguous ctrl stmts
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
    // Unambiguous comb stmts
    case node: StmtAssign  => kind(node)
    case node: StmtUpdate  => kind(node)
    case node: StmtPost    => kind(node)
    case node: StmtDecl    => kind(node)
    case node: StmtRead    => kind(node)
    case node: StmtWrite   => kind(node)
    case node: StmtComment => kind(node)
    case node: StmtStall   => kind(node)
    //
    case node: StmtError => kind(node)
    //
    case node: StmtGen => unreachable
  }

  private def kind(node: StmtBlock) = {
    if (node.body.nonEmpty) node.body.last.tpe else TypeCombStmt
  }

  private def kind(node: StmtIf) = node match {
    case StmtIf(_, Nil, Nil) => TypeCombStmt
    case StmtIf(_, ts, Nil)  => ts.last.tpe
    case StmtIf(_, _, es)    => es.last.tpe
  }

  private def kind(node: StmtCase) = {
    val stmts = node.cases.head.stmts
    if (stmts.nonEmpty) stmts.last.tpe else TypeCombStmt
  }

  private def kind(node: StmtExpr) = node.expr match {
    case ExprCall(target, _) =>
      target.tpe match {
        case _: TypeCtrlFunc => TypeCtrlStmt
        case _               => TypeCombStmt
      }
    case _ => TypeCombStmt
  }

  private def kind(node: StmtLoop) = TypeCtrlStmt
  private def kind(node: StmtWhile) = TypeCtrlStmt
  private def kind(node: StmtFor) = TypeCtrlStmt
  private def kind(node: StmtDo) = TypeCtrlStmt
  private def kind(node: StmtLet) = {
    // Note this is really redundant at the moment as due to syntax let
    // statements are always control statements, but is here for completeness
    if (node.body.nonEmpty) node.body.last.tpe else TypeCombStmt
  }
  private def kind(node: StmtFence) = TypeCtrlStmt
  private def kind(node: StmtBreak) = TypeCtrlStmt
  private def kind(node: StmtContinue) = TypeCtrlStmt
  private def kind(node: StmtGoto) = TypeCtrlStmt
  private def kind(node: StmtReturn) = TypeCtrlStmt

  private def kind(node: StmtAssign) = TypeCombStmt
  private def kind(node: StmtUpdate) = TypeCombStmt
  private def kind(node: StmtPost) = TypeCombStmt
  private def kind(node: StmtDecl) = TypeCombStmt
  private def kind(node: StmtRead) = TypeCombStmt
  private def kind(node: StmtWrite) = TypeCombStmt
  private def kind(node: StmtComment) = TypeCombStmt
  private def kind(node: StmtStall) = TypeCombStmt

  private def kind(node: StmtError) = TypeError // TODO: is this the righttype?

  //////////////////////////////////////////////////////////////////////////////
  // Typing Expr nodes
  //////////////////////////////////////////////////////////////////////////////

  private def kind(tree: Expr)(implicit cc: CompilerContext): Type = tree match {
    case node: ExprCall    => kind(node)
    case node: ExprUnary   => kind(node)
    case node: ExprBinary  => kind(node)
    case node: ExprTernary => kind(node)
    case node: ExprCat     => kind(node)
    case node: ExprRep     => kind(node)
    case node: ExprIndex   => kind(node)
    case node: ExprSlice   => kind(node)
    case node: ExprSelect  => kind(node)
    case node: ExprInt     => kind(node)
    case node: ExprStr     => kind(node)
    case node: ExprRef     => kind(node)
    case node: ExprType    => kind(node)
    case node: ExprError   => kind(node)
    case node: ExprNum     => kind(node)
    case node: ExprCast    => kind(node)
    case _: ExprIdent      => unreachable
  }

  private def kind(node: ExprError) = TypeError

  private def kind(node: ExprInt)(implicit cc: CompilerContext) = {
    TypeInt(node.signed, Expr(node.width) regularize node.loc)
  }

  private def kind(node: ExprNum) = TypeNum(node.signed)

  private def kind(node: ExprStr) = TypeStr

  private def kind(node: ExprRef) = {
    val ExprRef(symbol) = node
    val tpe = if (symbol == ErrorSymbol) {
      TypeError
    } else {
      symbol.kind match {
        // TODO: lose these
        case TypeParam(kind)    => kind
        case TypeConst(kind)    => kind
        case TypePipeline(kind) => kind
        case other              => other
      }
    }
    if (symbol.isTermSymbol) tpe else TypeType(tpe)
  }

  private def kind(node: ExprUnary)(implicit cc: CompilerContext) = node.op match {
    case "'"             => cc.ice("TypeAssigner invoked on unary ' operator")
    case "+" | "-" | "~" => node.expr.tpe
    case _               => TypeUInt(Expr(1) regularize node.loc)
  }

  private def kind(node: ExprBinary)(implicit cc: CompilerContext) = node.op match {
    case ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||" =>
      TypeUInt(Expr(1) regularize node.loc)
    case "<<" | ">>" | "<<<" | ">>>" => node.lhs.tpe
    case _ =>
      val lTpe = node.lhs.tpe
      val rTpe = node.rhs.tpe
      val signed = lTpe.isSigned && rTpe.isSigned
      if (lTpe.underlying.isNum && rTpe.underlying.isNum) {
        TypeNum(signed)
      } else {
        val width = if (lTpe.underlying.isNum) rTpe.width else lTpe.width
        TypeInt(signed, apply(Expr(width) withLoc node.loc))
      }
  }

  private def kind(node: ExprTernary)(implicit cc: CompilerContext) = {
    val tTpe = node.thenExpr.tpe
    val eTpe = node.elseExpr.tpe
    val signed = tTpe.isSigned && eTpe.isSigned
    if (tTpe.underlying.isNum && eTpe.underlying.isNum) {
      TypeNum(signed)
    } else {
      val width = if (tTpe.underlying.isNum) eTpe.width else tTpe.width
      TypeInt(signed, apply(Expr(width) withLoc node.loc))
    }
  }

  private def kind(node: ExprCat)(implicit cc: CompilerContext) = {
    // TODO: this probably should stay symbolic by casting all part so NUM
    // and then building the sum, that would allow it to work prior to parameter
    // specialization (i.e.: in const and param type parameters)
    val width = if (node.parts.lengthCompare(2) >= 0) {
      node.parts map { _.tpe.width } sum
    } else {
      node.parts.head.tpe.width
    }
    TypeUInt(Expr(width) regularize node.loc)
  }

  private def kind(node: ExprRep)(implicit cc: CompilerContext) = {
    // TODO: this probably should stay symbolic by casting all part so NUM
    // and then building the sum, that would allow it to work prior to parameter
    // specialization (i.e.: in const and param type parameters)
    val width = node.count.value.get * node.expr.tpe.width
    TypeUInt(Expr(width) regularize node.loc)
  }

  private def kind(node: ExprIndex)(implicit cc: CompilerContext) = {
    node.expr.tpe.underlying match {
      case _: TypeNum          => TypeUInt(Expr(1) regularize node.index.loc)
      case _: TypeInt          => TypeUInt(Expr(1) regularize node.index.loc)
      case TypeArray(kind, _)  => kind
      case TypeVector(kind, _) => kind
      case _                   => unreachable
    }
  }

  private def kind(node: ExprSlice)(implicit cc: CompilerContext) = {
    // TODO: implement vector slicing properly
    val width = if (node.op == ":") node.lidx - node.ridx + 1 else node.ridx
    width regularize node.loc
    TypeUInt(width.simplify)
  }

  private def kind(node: ExprSelect) = node.expr.tpe match {
    case TypeType(kind: CompoundType) => TypeType(kind(node.selector).get)
    case tpe: CompoundType            => tpe(node.selector).get
    case _                            => TypeError
  }

  private def kind(node: ExprType) = TypeType(node.kind)

  private def kind(node: ExprCall) = node.expr.tpe match {
    case TypeCombFunc(_, returnType) => returnType
    case TypeCtrlFunc(_, returnType) => returnType
    case TypePolyFunc(resolver)      => resolver(node.args).get.kind.asInstanceOf[TypeCombFunc].retType
    case _                           => unreachable
  }

  private def kind(node: ExprCast) = node.kind

  //////////////////////////////////////////////////////////////////////////////
  // 'apply' methods propagate type errors or assign the computed type. There
  // are lots of overloads of these to use static dispatch wherever possible.
  //////////////////////////////////////////////////////////////////////////////

  private def assign(tree: Tree)(kind: => Type): tree.type = {
    require(!tree.hasTpe)
    def hasError(node: TreeLike): Boolean = node.children exists {
      case child: Connect if !child.hasTpe => false
      case child: Tree if !child.hasTpe    => unreachable
      case child: Tree                     => child.tpe.isError
      case child: Type                     => child.children exists hasError
      case _                               => unreachable
    }
    val tpe = if (hasError(tree)) TypeError else kind
    tree withTpe tpe
  }

  def apply(node: Tree)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))

  // Other
  def apply(node: Entity): node.type = assign(node)(kind(node))
  def apply(node: Decl): node.type = assign(node)(kind(node))
  def apply(node: Instance): node.type = assign(node)(kind(node))
  def apply(node: Connect): node.type = assign(node)(kind(node))
  def apply(node: Function): node.type = assign(node)(kind(node))
  def apply(node: State): node.type = assign(node)(kind(node))
  def apply(node: TypeDefinition): node.type = assign(node)(kind(node))
  def apply(node: Thicket): node.type = assign(node)(kind(node))
  def apply(node: RegularCase): node.type = assign(node)(kind(node))
  def apply(node: DefaultCase): node.type = assign(node)(kind(node))
  def apply(node: Sym): node.type = assign(node)(kind(node))

  // Stmt
  def apply(node: Stmt): node.type = assign(node)(kind(node))
  def apply(node: StmtBlock): node.type = assign(node)(kind(node))
  def apply(node: StmtIf): node.type = assign(node)(kind(node))
  def apply(node: StmtCase): node.type = assign(node)(kind(node))
  def apply(node: StmtExpr): node.type = assign(node)(kind(node))
  def apply(node: StmtLoop): node.type = assign(node)(kind(node))
  def apply(node: StmtWhile): node.type = assign(node)(kind(node))
  def apply(node: StmtFor): node.type = assign(node)(kind(node))
  def apply(node: StmtDo): node.type = assign(node)(kind(node))
  def apply(node: StmtLet): node.type = assign(node)(kind(node))
  def apply(node: StmtFence): node.type = assign(node)(kind(node))
  def apply(node: StmtBreak): node.type = assign(node)(kind(node))
  def apply(node: StmtContinue): node.type = assign(node)(kind(node))
  def apply(node: StmtGoto): node.type = assign(node)(kind(node))
  def apply(node: StmtReturn): node.type = assign(node)(kind(node))
  def apply(node: StmtAssign): node.type = assign(node)(kind(node))
  def apply(node: StmtUpdate): node.type = assign(node)(kind(node))
  def apply(node: StmtPost): node.type = assign(node)(kind(node))
  def apply(node: StmtDecl): node.type = assign(node)(kind(node))
  def apply(node: StmtRead): node.type = assign(node)(kind(node))
  def apply(node: StmtWrite): node.type = assign(node)(kind(node))
  def apply(node: StmtComment): node.type = assign(node)(kind(node))
  def apply(node: StmtStall): node.type = assign(node)(kind(node))
  def apply(node: StmtError): node.type = assign(node)(kind(node))

  // Expr
  def apply(node: Expr)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprInt)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprNum): node.type = assign(node)(kind(node))
  def apply(node: ExprStr): node.type = assign(node)(kind(node))
  def apply(node: ExprRef): node.type = assign(node)(kind(node))
  def apply(node: ExprUnary)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprBinary)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprTernary)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprCat)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprRep)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprIndex)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprSlice)(implicit cc: CompilerContext): node.type = assign(node)(kind(node))
  def apply(node: ExprSelect): node.type = assign(node)(kind(node))
  def apply(node: ExprType): node.type = assign(node)(kind(node))
  def apply(node: ExprCall): node.type = assign(node)(kind(node))
  def apply(node: ExprCast): node.type = assign(node)(kind(node))
  def apply(node: ExprError): node.type = assign(node)(kind(node))
}
