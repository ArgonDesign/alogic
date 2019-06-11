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
import com.argondesign.alogic.util.unreachable

import scala.language.postfixOps

object TypeAssigner {

  def apply(tree: Tree)(implicit cc: CompilerContext): tree.type = {
    require(!tree.hasTpe)
    tree match {
      case node: Expr           => apply(node)
      case node: Stmt           => apply(node)
      case node: RegularCase    => apply(node)
      case node: DefaultCase    => apply(node)
      case node: Entity         => apply(node)
      case node: Decl           => apply(node)
      case node: Instance       => apply(node)
      case node: Connect        => apply(node)
      case node: Function       => apply(node)
      case node: State          => apply(node)
      case node: Sym            => apply(node)
      case node: TypeDefinition => apply(node)
      case node: Thicket        => apply(node)
      case _                    => unreachable
    }
    assert(tree.hasTpe)
    tree
  }

  //////////////////////////////////////////////////////////////////////////////
  // Typing Misc nodes
  //////////////////////////////////////////////////////////////////////////////

  private def assignTypeMisc(node: Tree): node.type = {
    require(!node.hasTpe)
    node withTpe TypeMisc
  }

  def apply(node: Entity): node.type = assignTypeMisc(node)
  def apply(node: Decl): node.type = assignTypeMisc(node)
  def apply(node: Instance): node.type = assignTypeMisc(node)
  def apply(node: Connect): node.type = assignTypeMisc(node)
  def apply(node: Function): node.type = assignTypeMisc(node)
  def apply(node: State): node.type = assignTypeMisc(node)
  def apply(node: TypeDefinition): node.type = assignTypeMisc(node)

  def apply(node: Thicket): node.type = {
    require(!node.hasTpe)
    assignTypeMisc(node)
  }

  def apply(node: RegularCase): node.type = {
    if (node.stmt.tpe == TypeError || (node.cond exists { _.tpe == TypeError })) {
      node withTpe TypeError
    } else {
      assignTypeMisc(node)
    }
  }

  def apply(node: DefaultCase): node.type = {
    if (node.stmt.tpe == TypeError) {
      node withTpe TypeError
    } else {
      assignTypeMisc(node)
    }
  }

  def apply(node: Sym): node.type = {
    require(!node.hasTpe)
    if (node.symbol == ErrorSymbol) {
      node withTpe TypeError
    } else {
      node withTpe node.symbol.kind
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Typing Stmt nodes
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: Stmt): tree.type = {
    require(!tree.hasTpe)
    tree match {
      case node: StmtBlock => apply(node)
      case node: StmtIf    => apply(node)
      case node: StmtCase  => apply(node)
      case node: StmtExpr  => apply(node)
      // Unambiguous ctrl stmts
      case node: StmtLoop     => apply(node)
      case node: StmtWhile    => apply(node)
      case node: StmtFor      => apply(node)
      case node: StmtDo       => apply(node)
      case node: StmtLet      => apply(node)
      case node: StmtFence    => apply(node)
      case node: StmtBreak    => apply(node)
      case node: StmtContinue => apply(node)
      case node: StmtGoto     => apply(node)
      case node: StmtReturn   => apply(node)
      // Unambiguous comb stmts
      case node: StmtAssign  => apply(node)
      case node: StmtUpdate  => apply(node)
      case node: StmtPost    => apply(node)
      case node: StmtDecl    => apply(node)
      case node: StmtRead    => apply(node)
      case node: StmtWrite   => apply(node)
      case node: StmtComment => apply(node)
      case node: StmtStall   => apply(node)
      //
      case node: StmtError => apply(node)
      //
      case _ => unreachable
    }
    tree
  }

  def apply(node: StmtBlock): node.type = {
    require(!node.hasTpe)
    val tpe = if (node.body.nonEmpty) node.body.last.tpe else TypeCombStmt
    node withTpe tpe
  }

  def apply(node: StmtIf): node.type = {
    require(!node.hasTpe)
    node withTpe node.thenStmt.tpe
  }

  def apply(node: StmtCase): node.type = {
    require(!node.hasTpe)
    node withTpe node.cases.head.stmt.tpe
  }

  def apply(node: StmtExpr): node.type = {
    require(!node.hasTpe)
    val tpe = node.expr match {
      case ExprCall(target, _) => {
        target.tpe match {
          case _: TypeCombFunc => TypeCombStmt
          case _: TypeCtrlFunc => TypeCtrlStmt
          case _               => unreachable
        }
      }
      case _ => TypeCombStmt
    }
    node withTpe tpe
  }

  def apply(node: StmtLoop): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtWhile): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtFor): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtDo): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtLet): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtFence): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtBreak): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtContinue): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtGoto): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtReturn): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtAssign): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtUpdate): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtPost): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtDecl): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtRead): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtWrite): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtComment): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtStall): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCombStmt
  }

  def apply(node: StmtError): node.type = {
    require(!node.hasTpe)
    node withTpe TypeError
  }

  //////////////////////////////////////////////////////////////////////////////
  // Typing Expr nodes
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: Expr)(implicit cc: CompilerContext): tree.type = {
    require(!tree.hasTpe)
    tree match {
      case node: ExprCall    => apply(node)
      case node: ExprUnary   => apply(node)
      case node: ExprBinary  => apply(node)
      case node: ExprTernary => apply(node)
      case node: ExprCat     => apply(node)
      case node: ExprRep     => apply(node)
      case node: ExprIndex   => apply(node)
      case node: ExprSlice   => apply(node)
      case node: ExprSelect  => apply(node)
      case node: ExprInt     => apply(node)
      case node: ExprStr     => apply(node)
      case node: ExprRef     => apply(node)
      case node: ExprType    => apply(node)
      case node: ExprError   => apply(node)
      case node: ExprNum     => apply(node)
      case node: ExprCast    => apply(node)
      case _                 => unreachable
    }
    tree
  }

  def apply(node: ExprError): node.type = node withTpe TypeError

  def apply(node: ExprInt)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val widthExpr = Expr(node.width) regularize node.loc
    node withTpe TypeInt(node.signed, widthExpr)
  }

  def apply(node: ExprNum): node.type = {
    require(!node.hasTpe)
    node withTpe TypeNum(node.signed)
  }

  def apply(node: ExprStr): node.type = node withTpe TypeStr

  def apply(node: ExprRef): node.type = {
    require(!node.hasTpe)
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
    val finalTpe = if (symbol.isTermSymbol) tpe else TypeType(tpe)
    node withTpe finalTpe
  }

  def apply(node: ExprUnary)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tpe = node.op match {
      case "+" | "-" | "~" => node.expr.tpe
      case _               => TypeUInt(Expr(1) regularize node.loc)
    }
    node withTpe tpe
  }

  def apply(node: ExprBinary)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tpe = node.op match {
      case ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||" =>
        TypeUInt(Expr(1) regularize node.loc)
      case "<<" | ">>" | "<<<" | ">>>" => node.lhs.tpe
      case _ => {
        val lTpe = node.lhs.tpe
        val rTpe = node.rhs.tpe
        val signed = lTpe.isSigned && rTpe.isSigned
        if (lTpe.isNum && rTpe.isNum) {
          TypeNum(signed)
        } else {
          val width = lTpe.width max rTpe.width
          TypeInt(signed, Expr(width) regularize node.loc)
        }
      }
    }
    node withTpe tpe
  }

  def apply(node: ExprTernary)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tTpe = node.thenExpr.tpe
    val eTpe = node.elseExpr.tpe
    val signed = tTpe.isSigned && eTpe.isSigned
    val tpe = if (tTpe.isNum && eTpe.isNum) {
      TypeNum(signed)
    } else {
      val width = tTpe.width max eTpe.width
      TypeInt(signed, Expr(width) regularize node.loc)
    }
    node withTpe tpe
  }

  def apply(node: ExprCat)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val width = if (node.parts.lengthCompare(2) >= 0) {
      node.parts map { _.tpe.width } sum
    } else {
      node.parts.head.tpe.width
    }
    node withTpe TypeUInt(Expr(width) regularize node.loc)
  }

  def apply(node: ExprRep)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val width = node.count * node.expr.tpe.width
    node withTpe TypeUInt(width.simplify regularize node.loc)
  }

  def apply(node: ExprIndex)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tpe = node.expr.tpe.underlying match {
      case _: TypeNum          => TypeUInt(Expr(1) regularize node.index.loc)
      case _: TypeInt          => TypeUInt(Expr(1) regularize node.index.loc)
      case TypeArray(kind, _)  => kind
      case TypeVector(kind, _) => kind
      case _                   => unreachable
    }
    node withTpe tpe
  }

  def apply(node: ExprSlice)(implicit cc: CompilerContext): node.type = {
    // TODO: implement vector slicing properly
    require(!node.hasTpe)
    val width = if (node.op == ":") node.lidx - node.ridx + 1 else node.ridx
    node withTpe TypeUInt(width.simplify regularize node.loc)
  }

  def apply(node: ExprSelect): node.type = {
    require(!node.hasTpe)
    val tpe = node.expr.tpe match {
      case TypeType(kind: CompoundType) => TypeType(kind(node.selector).get)
      case tpe: CompoundType            => tpe(node.selector).get
      case _                            => TypeError
    }
    node withTpe tpe
  }

  def apply(node: ExprType): node.type = {
    node withTpe TypeType(node.kind)
  }

  def apply(node: ExprCall): node.type = {
    val tpe = node.expr.tpe match {
      case TypeCombFunc(_, returnType) => returnType
      case TypeCtrlFunc(_, returnType) => returnType
      case _                           => unreachable
    }
    node withTpe tpe
  }

  def apply(node: ExprCast): node.type = {
    require(!node.hasTpe)
    node withTpe node.kind
  }
}
