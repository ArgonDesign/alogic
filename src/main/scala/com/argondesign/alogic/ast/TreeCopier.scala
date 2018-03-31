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
// A copy on write Tree copier used in tree transformations
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import Trees._

// scalastyle:off token

// Given a Tree and new child nodes, create new Tree if children are not the same as
// the children of the current Tree, otherwise reuse existing Tree. Note that only
// fields containing Tree instances are checked as other fields are not transformed
// recursively in TreeTransformer
object TreeCopier {

  final def apply(tree: Root)(typeDefinitions: List[Tree], entity: Tree): Root = {
    if ((typeDefinitions eq tree.typeDefinitions) && (entity eq tree.entity)) {
      tree
    } else {
      assert(typeDefinitions forall { _.isInstanceOf[TypeDefinition] })
      Root(
        typeDefinitions.asInstanceOf[List[TypeDefinition]],
        entity.asInstanceOf[Entity]
      ) withLoc tree.loc
    }
  }

  final def apply(tree: TypeDefinitionStruct)(ref: Tree): TypeDefinitionStruct = {
    if (ref eq tree.ref) {
      tree
    } else {
      TypeDefinitionStruct(ref.asInstanceOf[Ref], tree.fieldNames, tree.fieldTypes) withLoc tree.loc
    }
  }

  final def apply(tree: TypeDefinitionTypedef)(ref: Tree): TypeDefinitionTypedef = {
    if (ref eq tree.ref) {
      tree
    } else {
      TypeDefinitionTypedef(ref.asInstanceOf[Ref], tree.kind) withLoc tree.loc
    }
  }

  final def apply(tree: Entity)(
      ref: Tree,
      declarations: List[Tree],
      instances: List[Tree],
      connects: List[Tree],
      functions: List[Tree],
      states: List[Tree],
      fenceStmts: List[Tree],
      entities: List[Tree]
  ): Entity = {
    if ((ref eq tree.ref) &&
        (declarations eq tree.declarations) &&
        (instances eq tree.instances) &&
        (connects eq tree.connects) &&
        (functions eq tree.functions) &&
        (states eq tree.states) &&
        (fenceStmts eq tree.fenceStmts) &&
        (entities eq tree.entities)) {
      tree
    } else {
      assert(declarations forall { _.isInstanceOf[Decl] })
      assert(instances forall { _.isInstanceOf[Instance] })
      assert(connects forall { _.isInstanceOf[Connect] })
      assert(functions forall { _.isInstanceOf[Function] })
      assert(states forall { _.isInstanceOf[State] })
      assert(fenceStmts forall { _.isInstanceOf[Stmt] })
      assert(entities forall { _.isInstanceOf[Entity] })
      Entity(
        ref.asInstanceOf[Ref],
        declarations.asInstanceOf[List[Decl]],
        instances.asInstanceOf[List[Instance]],
        connects.asInstanceOf[List[Connect]],
        functions.asInstanceOf[List[Function]],
        states.asInstanceOf[List[State]],
        fenceStmts.asInstanceOf[List[Stmt]],
        entities.asInstanceOf[List[Entity]],
        tree.verbatim
      ) withLoc tree.loc withVariant tree.variant
    }
  }

  final def apply(tree: Decl)(ref: Tree, init: Option[Tree]): Decl = {
    if ((ref eq tree.ref) && (tree.init eq init)) {
      tree
    } else {
      assert(init forall { _.isInstanceOf[Expr] })
      Decl(ref.asInstanceOf[Ref], tree.kind, init.asInstanceOf[Option[Expr]]) withLoc tree.loc
    }
  }

  final def apply(tree: Instance)(ref: Tree, module: Tree, paramExprs: List[Tree]): Instance = {
    if ((ref eq tree.ref) && (module eq tree.module) && (paramExprs eq tree.paramExprs)) {
      tree
    } else {
      assert(paramExprs forall { _.isInstanceOf[Expr] })
      Instance(
        ref.asInstanceOf[Ref],
        module.asInstanceOf[Ref],
        tree.paramNames,
        paramExprs.asInstanceOf[List[Expr]]
      ) withLoc tree.loc
    }
  }

  final def apply(tree: Connect)(lhs: Tree, rhs: List[Tree]): Connect = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      assert(rhs forall { _.isInstanceOf[Expr] })
      Connect(lhs.asInstanceOf[Expr], rhs.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  final def apply(tree: Function)(ref: Tree, body: List[Tree]): Function = {
    if ((ref eq tree.ref) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      Function(ref.asInstanceOf[Ref], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  final def apply(tree: State)(ref: Tree, body: List[Tree]): State = {
    if ((ref eq tree.ref) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      State(ref.asInstanceOf[Ref], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtBlock)(body: List[Tree]): StmtBlock = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtBlock(body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtIf)(cond: Tree, thenStmt: Tree, elseStmt: Option[Tree]): StmtIf = {
    if ((cond eq tree.cond) && (thenStmt eq tree.thenStmt) && (elseStmt eq tree.elseStmt)) {
      tree
    } else {
      assert(elseStmt forall { _.isInstanceOf[Stmt] })
      StmtIf(
        cond.asInstanceOf[Expr],
        thenStmt.asInstanceOf[Stmt],
        elseStmt.asInstanceOf[Option[Stmt]]
      ) withLoc tree.loc
    }
  }

  final def apply(tree: StmtCase)(expr: Tree, cases: List[Tree], default: List[Tree]): StmtCase = {
    if ((expr eq tree.expr) && (cases eq tree.cases) && (default eq tree.default)) {
      tree
    } else {
      assert(cases forall { _.isInstanceOf[CaseClause] })
      assert(default forall { _.isInstanceOf[Stmt] })
      StmtCase(
        expr.asInstanceOf[Expr],
        cases.asInstanceOf[List[CaseClause]],
        default.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  final def apply(tree: CaseClause)(cond: List[Tree], body: Tree): CaseClause = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      assert(cond forall { _.isInstanceOf[Expr] })
      CaseClause(cond.asInstanceOf[List[Expr]], body.asInstanceOf[Stmt]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtLoop)(body: List[Tree]): StmtLoop = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtLoop(body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtWhile)(cond: Tree, body: List[Tree]): StmtWhile = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtWhile(cond.asInstanceOf[Expr], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  final def apply(
      tree: StmtFor
  )(
      inits: List[Tree],
      cond: Option[Tree],
      step: List[Tree],
      body: List[Tree]
  ): StmtFor = {
    if ((inits eq tree.inits) && (cond eq tree.cond) && (step eq tree.step) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall { _.isInstanceOf[Stmt] })
      assert(cond forall { _.isInstanceOf[Expr] })
      assert(step forall { _.isInstanceOf[Stmt] })
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtFor(
        inits.asInstanceOf[List[Stmt]],
        cond.asInstanceOf[Option[Expr]],
        step.asInstanceOf[List[Stmt]],
        body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  final def apply(tree: StmtDo)(cond: Tree, body: List[Tree]): StmtDo = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtDo(cond.asInstanceOf[Expr], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtLet)(inits: List[Tree], body: Tree): StmtLet = {
    if ((inits eq tree.inits) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall { _.isInstanceOf[Stmt] })
      StmtLet(inits.asInstanceOf[List[Stmt]], body.asInstanceOf[Stmt]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtGoto)(expr: Tree): StmtGoto = {
    if (expr eq tree.expr) {
      tree
    } else {
      StmtGoto(expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtAssign)(lhs: Tree, rhs: Tree): StmtAssign = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      StmtAssign(lhs.asInstanceOf[Expr], rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtUpdate)(lhs: Tree, rhs: Tree): StmtUpdate = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      StmtUpdate(lhs.asInstanceOf[Expr], tree.op, rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtPost)(expr: Tree): StmtPost = {
    if (expr eq tree.expr) {
      tree
    } else {
      StmtPost(expr.asInstanceOf[Expr], tree.op) withLoc tree.loc
    }
  }

  final def apply(tree: StmtExpr)(expr: Tree): StmtExpr = {
    if (expr eq tree.expr) {
      tree
    } else {
      StmtExpr(expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: StmtDecl)(decl: Tree): StmtDecl = {
    if (decl eq tree.decl) {
      tree
    } else {
      StmtDecl(decl.asInstanceOf[Decl]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprCall)(expr: Tree, args: List[Tree]): ExprCall = {
    if ((expr eq tree.expr) && (args eq tree.args)) {
      tree
    } else {
      assert(args forall { _.isInstanceOf[Expr] })
      ExprCall(expr.asInstanceOf[Expr], args.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprUnary)(expr: Tree): ExprUnary = {
    if (expr eq tree.expr) {
      tree
    } else {
      ExprUnary(tree.op, expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprBinary)(lhs: Tree, rhs: Tree): ExprBinary = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      ExprBinary(lhs.asInstanceOf[Expr], tree.op, rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprTernary)(cond: Tree, thenExpr: Tree, elseExpr: Tree): ExprTernary = {
    if ((cond eq tree.cond) && (thenExpr eq tree.thenExpr) && (elseExpr eq tree.elseExpr)) {
      tree
    } else {
      ExprTernary(cond.asInstanceOf[Expr], thenExpr.asInstanceOf[Expr], elseExpr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprRep)(count: Tree, expr: Tree): ExprRep = {
    if ((count eq tree.count) && (expr eq tree.expr)) {
      tree
    } else {
      ExprRep(count.asInstanceOf[Expr], expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprCat)(parts: List[Tree]): ExprCat = {
    if (parts eq tree.parts) {
      tree
    } else {
      assert(parts forall { _.isInstanceOf[Expr] })
      ExprCat(parts.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprIndex)(expr: Tree, index: Tree): ExprIndex = {
    if ((expr eq tree.expr) && (index eq tree.index)) {
      tree
    } else {
      ExprIndex(expr.asInstanceOf[Expr], index.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprSlice)(expr: Tree, lidx: Tree, ridx: Tree): ExprSlice = {
    if ((expr eq tree.expr) && (lidx eq tree.lidx) && (ridx eq tree.ridx)) {
      tree
    } else {
      ExprSlice(expr.asInstanceOf[Expr], lidx.asInstanceOf[Expr], tree.op, ridx.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  final def apply(tree: ExprSelect)(expr: Tree): ExprSelect = {
    if (expr eq tree.expr) {
      tree
    } else {
      ExprSelect(expr.asInstanceOf[Expr], tree.selector) withLoc tree.loc
    }
  }

  final def apply(tree: ExprRef)(ref: Tree): ExprRef = {
    if (ref eq tree.ref) {
      tree
    } else {
      ExprRef(ref.asInstanceOf[Ref]) withLoc tree.loc
    }
  }

  final def apply(tree: Thicket)(trees: List[Tree]): Thicket = {
    if (trees eq tree.trees) {
      tree
    } else {
      Thicket(trees) withLoc tree.loc
    }
  }

}
