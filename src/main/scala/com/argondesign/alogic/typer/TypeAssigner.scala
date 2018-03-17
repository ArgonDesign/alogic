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

object TypeAssigner {

  def apply(tree: Tree)(implicit cc: CompilerContext): tree.type = {
    require(!tree.hasTpe)
    tree match {
      case node: Expr           => apply(node)
      case node: Stmt           => apply(node)
      case node: CaseClause     => apply(node)
      case node: Entity         => apply(node)
      case node: Decl           => apply(node)
      case node: Instance       => apply(node)
      case node: Connect        => apply(node)
      case node: Function       => apply(node)
      case node: State          => apply(node)
      case node: Sym            => apply(node)
      case node: TypeDefinition => apply(node)
      case _                    => cc.ice(tree, s"Don't know how to type '${tree}")
    }
    assert(tree.hasTpe)
    assert(!tree.tpe.isInstanceOf[TypeRef])
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
  def apply(node: Sym): node.type = assignTypeMisc(node)
  def apply(node: TypeDefinition): node.type = assignTypeMisc(node)

  def apply(node: CaseClause): node.type = {
    if (node.body.tpe == TypeError || (node.cond exists { _.tpe == TypeError })) {
      node withTpe TypeError
    } else {
      assignTypeMisc(node)
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
      case node: StmtLoop   => apply(node)
      case node: StmtWhile  => apply(node)
      case node: StmtFor    => apply(node)
      case node: StmtDo     => apply(node)
      case node: StmtFence  => apply(node)
      case node: StmtBreak  => apply(node)
      case node: StmtGoto   => apply(node)
      case node: StmtReturn => apply(node)
      // Unambiguous comb stmts
      case node: StmtAssign        => apply(node)
      case node: StmtDecl          => apply(node)
      case node: StmtRead          => apply(node)
      case node: StmtWrite         => apply(node)
      case node: StmtDollarComment => apply(node)
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
    val child = if (node.cases.nonEmpty) {
      node.cases.head.body
    } else {
      node.default.last
    }
    node withTpe child.tpe
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

  def apply(node: StmtFence): node.type = {
    require(!node.hasTpe)
    node withTpe TypeCtrlStmt
  }

  def apply(node: StmtBreak): node.type = {
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

  def apply(node: StmtDollarComment): node.type = {
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
    }
    tree
  }

  def apply(node: ExprError): node.type = node withTpe TypeError

  def apply(node: ExprInt): node.type = {
    require(!node.hasTpe)
    val widthExpr = Expr(node.width) withLoc node.loc
    node withTpe TypeInt(node.signed, widthExpr)
  }

  def apply(node: ExprNum): node.type = {
    require(!node.hasTpe)
    node withTpe TypeNum(node.signed)
  }

  def apply(node: ExprStr): node.type = node withTpe TypeStr

  def apply(node: ExprRef)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val ExprRef(Sym(symbol)) = node
    val tpe = if (symbol == ErrorSymbol) {
      TypeError
    } else {
      symbol.denot.kind.chase match {
        case TypeParam(kind)    => kind
        case TypeConst(kind)    => kind
        case TypePipeline(kind) => kind
        case other              => other
      }
    }
    val finalTpe = if (symbol.isTermSymbol) tpe else TypeType(tpe)
    node withTpe finalTpe.chase
  }

  def apply(node: ExprUnary): node.type = {
    require(!node.hasTpe)
    val tpe = node.op match {
      case "+" | "-" | "~" => node.expr.tpe
      case _               => TypeUInt(Expr(1) withLoc node.loc)
    }
    node withTpe tpe
  }

  def apply(node: ExprBinary)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tpe = node.op match {
      case ">" | ">=" | "<" | "<=" | "==" | "!=" | "&&" | "||" =>
        TypeUInt(Expr(1) withLoc node.loc)
      case "<<" | ">>" | "<<<" | ">>>" => node.lhs.tpe
      case _ => {
        val lTpe = node.lhs.tpe
        val rTpe = node.rhs.tpe
        val signed = lTpe.isSigned && rTpe.isSigned
        val width = lTpe.width max rTpe.width
        TypeInt(signed, width.simplify)
      }
    }
    node withTpe tpe
  }

  def apply(node: ExprTernary)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tTpe = node.thenExpr.tpe
    val eTpe = node.elseExpr.tpe
    val signed = tTpe.isSigned && eTpe.isSigned
    val width = tTpe.width max eTpe.width
    node withTpe TypeInt(signed, width.simplify)
  }

  def apply(node: ExprCat)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val width = if (node.parts.lengthCompare(2) >= 0) {
      node.parts map { _.tpe.width } reduceLeft { _ + _ }
    } else {
      node.parts.head.tpe.width
    }
    node withTpe TypeUInt(width.simplify)
  }

  def apply(node: ExprRep)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val width = node.count * node.expr.tpe.width
    node withTpe TypeUInt(width.simplify)
  }

  def apply(node: ExprIndex): node.type = {
    require(!node.hasTpe)
    val tpe = node.expr.tpe.underlying match {
      case _: TypeInt          => TypeUInt(Expr(1) withLoc node.index.loc)
      case TypeArray(kind, _)  => kind
      case TypeVector(kind, _) => kind
      case _                   => unreachable
    }
    node withTpe tpe
  }

  def apply(node: ExprSlice)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val width = if (node.op == ":") node.lidx - node.ridx + 1 else node.ridx
    node withTpe TypeUInt(width.simplify)
  }

  def apply(node: ExprSelect)(implicit cc: CompilerContext): node.type = {
    require(!node.hasTpe)
    val tpe = node.expr.tpe.chase match {
      case TypeType(kind: CompoundType) => TypeType(kind(node.selector).get)
      case tpe: CompoundType            => tpe(node.selector).get
      case _                            => unreachable
    }
    node withTpe tpe.chase
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

}
