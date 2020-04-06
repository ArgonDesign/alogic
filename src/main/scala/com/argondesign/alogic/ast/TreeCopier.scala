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
// Tree copier used in tree transformations to only perform copying if required
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

// scalastyle:off token

// Given a Tree and new child nodes, create new Tree if children are not the same as
// the children of the current Tree, otherwise reuse existing Tree. Note that only
// fields containing Tree instances are checked as other fields are not transformed
// recursively in TreeTransformer
object TreeCopier {

  //////////////////////////////////////////////////////////////////////////////
  // Root
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: Root)(body: List[Tree]): Root = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Riz] })
      Root(body.asInstanceOf[List[Riz]]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Ref
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: Ident)(indices: List[Tree]): Ident = {
    if (indices eq tree.idxs) {
      tree
    } else {
      assert(indices forall { _.isInstanceOf[Expr] })
      Ident(tree.name, indices.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  def apply(tree: Sym)(indices: List[Tree]): Sym = {
    if (indices eq tree.idxs) {
      tree
    } else {
      assert(indices forall { _.isInstanceOf[Expr] })
      Sym(tree.symbol, indices.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Desc
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: DescVar)(ref: Tree, spec: Tree, initOpt: Option[Tree]): DescVar = {
    if ((ref eq tree.ref) && (spec eq tree.spec) && (initOpt eq tree.initOpt)) {
      tree
    } else {
      assert(initOpt forall { _.isInstanceOf[Expr] })
      DescVar(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr],
        initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescVal)(ref: Tree, spec: Tree, init: Tree): DescVal = {
    if ((ref eq tree.ref) && (spec eq tree.spec) && (init eq tree.init)) {
      tree
    } else {
      DescVal(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr],
        init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescIn)(ref: Tree, spec: Tree): DescIn = {
    if ((ref eq tree.ref) && (spec eq tree.spec)) {
      tree
    } else {
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescOut)(ref: Tree, spec: Tree, initOpt: Option[Tree]): DescOut = {
    if ((ref eq tree.ref) && (spec eq tree.spec) && (initOpt eq tree.initOpt)) {
      tree
    } else {
      assert(initOpt forall { _.isInstanceOf[Expr] })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        spec = spec.asInstanceOf[Expr],
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescPipeline)(ref: Tree, spec: Tree): DescPipeline = {
    if ((ref eq tree.ref) && (spec eq tree.spec)) {
      tree
    } else {
      DescPipeline(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescParam)(ref: Tree, spec: Tree, initOpt: Option[Tree]): DescParam = {
    if ((ref eq tree.ref) && (spec eq tree.spec) && (initOpt eq tree.initOpt)) {
      tree
    } else {
      assert(initOpt forall { _.isInstanceOf[Expr] })
      DescParam(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr],
        initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescConst)(ref: Tree, spec: Tree, init: Tree): DescConst = {
    if ((ref eq tree.ref) && (spec eq tree.spec) && (init eq tree.init)) {
      tree
    } else {
      DescConst(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr],
        init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescGen)(ref: Tree, spec: Tree, init: Tree): DescGen = {
    if ((ref eq tree.ref) && (spec eq tree.spec) && (init eq tree.init)) {
      tree
    } else {
      DescGen(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr],
        init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescArray)(ref: Tree, elem: Tree, size: Tree): DescArray = {
    if ((ref eq tree.ref) && (elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      DescArray(
        ref.asInstanceOf[Ref],
        elem.asInstanceOf[Expr],
        size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescSram)(ref: Tree, elem: Tree, size: Tree): DescSram = {
    if ((ref eq tree.ref) && (elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        elem = elem.asInstanceOf[Expr],
        size = size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescType)(ref: Tree, spec: Tree): DescType = {
    if ((ref eq tree.ref) && (spec eq tree.spec)) {
      tree
    } else {
      DescType(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescEntity)(ref: Tree, body: List[Tree]): DescEntity = {
    if ((ref eq tree.ref) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Ent] })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescRecord)(ref: Tree, body: List[Tree]): DescRecord = {
    if ((ref eq tree.ref) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Rec] })
      DescRecord(
        ref.asInstanceOf[Ref],
        body.asInstanceOf[List[Rec]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescInstance)(ref: Tree, spec: Tree): DescInstance = {
    if ((ref eq tree.ref) && (spec eq tree.spec)) {
      tree
    } else {
      DescInstance(
        ref.asInstanceOf[Ref],
        spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescSingleton)(ref: Tree, body: List[Tree]): DescSingleton = {
    if ((ref eq tree.ref) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Ent] })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescFunc)(ref: Tree, ret: Tree, args: List[Tree], body: List[Tree]): DescFunc = {
    if ((ref eq tree.ref) && (ret eq tree.ret) && (args eq tree.args) && (body eq tree.body)) {
      tree
    } else {
      assert(args forall { _.isInstanceOf[Desc] })
      assert(body forall { _.isInstanceOf[Stmt] })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        ret = ret.asInstanceOf[Expr],
        args = args.asInstanceOf[List[Desc]],
        body = body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescChoice)(ref: Tree, choices: List[Tree]): DescChoice = {
    if ((ref eq tree.ref) && (choices eq tree.choices)) {
      tree
    } else {
      assert(choices forall { _.isInstanceOf[ExprSym] })
      DescChoice(
        ref.asInstanceOf[Ref],
        choices.asInstanceOf[List[ExprSym]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Decl
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: DeclVar)(spec: Tree): DeclVar = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr],
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclVal)(spec: Tree): DeclVal = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr],
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclIn)(spec: Tree): DeclIn = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclOut)(spec: Tree): DeclOut = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclPipeline)(spec: Tree): DeclPipeline = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclConst)(spec: Tree): DeclConst = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclGen)(spec: Tree): DeclGen = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclArray)(elem: Tree, size: Tree): DeclArray = {
    if ((elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      tree.copy(
        elem = elem.asInstanceOf[Expr],
        size = size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclSram)(elem: Tree, size: Tree): DeclSram = {
    if ((elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      tree.copy(
        elem = elem.asInstanceOf[Expr],
        size = size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclStack)(elem: Tree, size: Tree): DeclStack = {
    if ((elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      tree.copy(
        elem = elem.asInstanceOf[Expr],
        size = size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclType)(spec: Tree): DeclType = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclEntity)(decls: List[Tree]): DeclEntity = {
    if (decls eq tree.decls) {
      tree
    } else {
      assert(decls forall { _.isInstanceOf[Decl] })
      tree.copy(
        decls = decls.asInstanceOf[List[Decl]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclRecord)(decls: List[Tree]): DeclRecord = {
    if (decls eq tree.decls) {
      tree
    } else {
      assert(decls forall { _.isInstanceOf[Decl] })
      tree.copy(
        decls = decls.asInstanceOf[List[Decl]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclInstance)(spec: Tree): DeclInstance = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclSingleton)(decls: List[Tree]): DeclSingleton = {
    if (decls eq tree.decls) {
      tree
    } else {
      assert(decls forall { _.isInstanceOf[Decl] })
      tree.copy(
        decls = decls.asInstanceOf[List[Decl]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclFunc)(ret: Tree, args: List[Tree]): DeclFunc = {
    if ((ret eq tree.ret) && (args eq tree.args)) {
      tree
    } else {
      assert(args forall { _.isInstanceOf[Decl] })
      tree.copy(
        ret = ret.asInstanceOf[Expr],
        args = args.asInstanceOf[List[Decl]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Defn
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: DefnVar)(initOpt: Option[Tree]): DefnVar = {
    if (initOpt eq tree.initOpt) {
      tree
    } else {
      assert(initOpt forall { _.isInstanceOf[Expr] })
      tree.copy(
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnVal)(init: Tree): DefnVal = {
    if (init eq tree.init) {
      tree
    } else {
      tree.copy(
        init = init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnOut)(initOpt: Option[Tree]): DefnOut = {
    if (initOpt eq tree.initOpt) {
      tree
    } else {
      assert(initOpt forall { _.isInstanceOf[Expr] })
      tree.copy(
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnConst)(init: Tree): DefnConst = {
    if (init eq tree.init) {
      tree
    } else {
      tree.copy(
        init = init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnGen)(init: Tree): DefnGen = {
    if (init eq tree.init) {
      tree
    } else {
      tree.copy(
        init = init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnEntity)(body: List[Tree]): DefnEntity = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Ent] })
      tree.copy(
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnRecord)(body: List[Tree]): DefnRecord = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Rec] })
      tree.copy(
        body = body.asInstanceOf[List[Rec]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnSingleton)(body: List[Tree]): DefnSingleton = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Ent] })
      tree.copy(
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnFunc)(args: List[Tree], body: List[Tree]): DefnFunc = {
    if ((args eq tree.args) && (body eq tree.body)) {
      tree
    } else {
      assert(args forall { _.isInstanceOf[Defn] })
      assert(body forall { _.isInstanceOf[Stmt] })
      tree.copy(
        args = args.asInstanceOf[List[Defn]],
        body = body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnState)(expr: Tree, body: List[Tree]): DefnState = {
    if ((expr eq tree.expr) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      tree.copy(
        expr = expr.asInstanceOf[Expr],
        body = body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Gen
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: GenIf)(cond: Tree, thenItems: List[Tree], elseItems: List[Tree]): GenIf = {
    if ((cond eq tree.cond) && (thenItems eq tree.thenItems) && (elseItems eq tree.elseItems)) {
      tree
    } else {
      GenIf(
        cond.asInstanceOf[Expr],
        thenItems.asInstanceOf[List[Stmt]],
        elseItems.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: GenFor
  )(
      inits: List[Tree],
      cond: Tree,
      steps: List[Tree],
      body: List[Tree]
  ): GenFor = {
    if ((inits eq tree.inits) && (cond eq tree.cond) && (steps eq tree.steps) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall { _.isInstanceOf[Stmt] })
      assert(steps forall { _.isInstanceOf[Stmt] })
      GenFor(
        inits.asInstanceOf[List[Stmt]],
        cond.asInstanceOf[Expr],
        steps.asInstanceOf[List[Stmt]],
        body
      ) withLoc tree.loc
    }
  }

  def apply(tree: GenRange)(inits: List[Tree], end: Tree, body: List[Tree]): GenRange = {
    if ((inits eq inits.init) && (end eq tree.end) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall { _.isInstanceOf[Stmt] })
      tree.copy(
        inits = inits.asInstanceOf[List[Stmt]],
        end = end.asInstanceOf[Expr],
        body = body
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Assertion
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: AssertionAssert)(cond: Tree): AssertionAssert = {
    if (cond eq tree.cond) {
      tree
    } else {
      tree.copy(cond = cond.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: AssertionAssume)(cond: Tree): AssertionAssume = {
    if (cond eq tree.cond) {
      tree
    } else {
      tree.copy(cond = cond.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: AssertionStatic)(cond: Tree): AssertionStatic = {
    if (cond eq tree.cond) {
      tree
    } else {
      tree.copy(cond = cond.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Riz
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: RizDesc)(desc: Tree): RizDesc = {
    if (desc eq tree.desc) {
      tree
    } else {
      RizDesc(desc.asInstanceOf[Desc]) withLoc tree.loc
    }
  }

  def apply(tree: RizDecl)(decl: Tree): RizDecl = {
    if (decl eq tree.decl) {
      tree
    } else {
      RizDecl(decl.asInstanceOf[Decl]) withLoc tree.loc
    }
  }

  def apply(tree: RizDefn)(defn: Tree): RizDefn = {
    if (defn eq tree.defn) {
      tree
    } else {
      RizDefn(defn.asInstanceOf[Defn]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Ent
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: EntDesc)(desc: Tree): EntDesc = {
    if (desc eq tree.desc) {
      tree
    } else {
      EntDesc(desc.asInstanceOf[Desc]) withLoc tree.loc
    }
  }

  def apply(tree: EntDecl)(decl: Tree): EntDecl = {
    if (decl eq tree.decl) {
      tree
    } else {
      EntDecl(decl.asInstanceOf[Decl]) withLoc tree.loc
    }
  }

  def apply(tree: EntDefn)(defn: Tree): EntDefn = {
    if (defn eq tree.defn) {
      tree
    } else {
      EntDefn(defn.asInstanceOf[Defn]) withLoc tree.loc
    }
  }

  def apply(tree: EntGen)(gen: Tree): EntGen = {
    if (gen eq tree.gen) {
      tree
    } else {
      EntGen(gen.asInstanceOf[Gen]) withLoc tree.loc
    }
  }

  def apply(tree: EntConnect)(lhs: Tree, rhs: List[Tree]): EntConnect = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      assert(rhs forall { _.isInstanceOf[Expr] })
      EntConnect(lhs.asInstanceOf[Expr], rhs.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  def apply(tree: EntCombProcess)(stmts: List[Tree]): EntCombProcess = {
    if (stmts eq tree.stmts) {
      tree
    } else {
      assert(stmts forall { _.isInstanceOf[Stmt] })
      EntCombProcess(stmts.asInstanceOf[List[StmtIf]]) withLoc tree.loc
    }
  }

  def apply(tree: EntClockedProcess)(clk: Tree,
                                     rstOpt: Option[Tree],
                                     stmts: List[Tree]): EntClockedProcess = {
    if ((clk eq tree.clk) && (rstOpt eq tree.rstOpt) && (stmts eq tree.stmts)) {
      tree
    } else {
      assert(rstOpt forall { _.isInstanceOf[Expr] })
      assert(stmts forall { _.isInstanceOf[Stmt] })
      EntClockedProcess(
        clk.asInstanceOf[Expr],
        rstOpt.asInstanceOf[Option[Expr]],
        stmts.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: EntAssertion)(assertion: Tree): EntAssertion = {
    if (assertion eq tree.assertion) {
      tree
    } else {
      EntAssertion(assertion.asInstanceOf[Assertion]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Rec
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: RecDesc)(desc: Tree): RecDesc = {
    if (desc eq tree.desc) {
      tree
    } else {
      RecDesc(desc.asInstanceOf[Desc]) withLoc tree.loc
    }
  }

  def apply(tree: RecDecl)(decl: Tree): RecDecl = {
    if (decl eq tree.decl) {
      tree
    } else {
      RecDecl(decl.asInstanceOf[Decl]) withLoc tree.loc
    }
  }

  def apply(tree: RecDefn)(defn: Tree): RecDefn = {
    if (defn eq tree.defn) {
      tree
    } else {
      RecDefn(defn.asInstanceOf[Defn]) withLoc tree.loc
    }
  }

  def apply(tree: RecGen)(gen: Tree): RecGen = {
    if (gen eq tree.gen) {
      tree
    } else {
      RecGen(gen.asInstanceOf[Gen]) withLoc tree.loc
    }
  }

  def apply(tree: RecAssertion)(assertion: Tree): RecAssertion = {
    if (assertion eq tree.assertion) {
      tree
    } else {
      RecAssertion(assertion.asInstanceOf[Assertion]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Stmt
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: StmtDesc)(desc: Tree): StmtDesc = {
    if (desc eq tree.desc) {
      tree
    } else {
      StmtDesc(desc.asInstanceOf[Desc]) withLoc tree.loc
    }
  }

  def apply(tree: StmtDecl)(decl: Tree): StmtDecl = {
    if (decl eq tree.decl) {
      tree
    } else {
      StmtDecl(decl.asInstanceOf[Decl]) withLoc tree.loc
    }
  }

  def apply(tree: StmtDefn)(defn: Tree): StmtDefn = {
    if (defn eq tree.defn) {
      tree
    } else {
      StmtDefn(defn.asInstanceOf[Defn]) withLoc tree.loc
    }
  }

  def apply(tree: StmtGen)(gen: Tree): StmtGen = {
    if (gen eq tree.gen) {
      tree
    } else {
      StmtGen(gen.asInstanceOf[Gen]) withLoc tree.loc
    }
  }

  def apply(tree: StmtBlock)(body: List[Tree]): StmtBlock = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtBlock(body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: StmtIf)(
      cond: Tree,
      thenStmts: List[Tree],
      elseStmts: List[Tree]
  ): StmtIf = {
    if ((cond eq tree.cond) && (thenStmts eq tree.thenStmts) && (elseStmts eq tree.elseStmts)) {
      tree
    } else {
      assert(thenStmts forall { _.isInstanceOf[Stmt] })
      assert(elseStmts forall { _.isInstanceOf[Stmt] })
      StmtIf(
        cond.asInstanceOf[Expr],
        thenStmts.asInstanceOf[List[Stmt]],
        elseStmts.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: StmtCase)(expr: Tree, cases: List[Tree]): StmtCase = {
    if ((expr eq tree.expr) && (cases eq tree.cases)) {
      tree
    } else {
      assert(cases forall { _.isInstanceOf[Case] })
      StmtCase(
        expr.asInstanceOf[Expr],
        cases.asInstanceOf[List[Case]],
      ) withLoc tree.loc
    }
  }

  def apply(tree: StmtLoop)(body: List[Tree]): StmtLoop = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtLoop(body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: StmtWhile)(cond: Tree, body: List[Tree]): StmtWhile = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtWhile(cond.asInstanceOf[Expr], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(
      tree: StmtFor
  )(
      inits: List[Tree],
      cond: Option[Tree],
      steps: List[Tree],
      body: List[Tree]
  ): StmtFor = {
    if ((inits eq tree.inits) && (cond eq tree.cond) && (steps eq tree.steps) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall { _.isInstanceOf[Stmt] })
      assert(cond forall { _.isInstanceOf[Expr] })
      assert(steps forall { _.isInstanceOf[Stmt] })
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtFor(
        inits.asInstanceOf[List[Stmt]],
        cond.asInstanceOf[Option[Expr]],
        steps.asInstanceOf[List[Stmt]],
        body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: StmtDo)(cond: Tree, body: List[Tree]): StmtDo = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtDo(cond.asInstanceOf[Expr], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: StmtLet)(inits: List[Tree], body: List[Tree]): StmtLet = {
    if ((inits eq tree.inits) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall { _.isInstanceOf[Stmt] })
      assert(body forall { _.isInstanceOf[Stmt] })
      StmtLet(inits.asInstanceOf[List[Stmt]], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: StmtGoto)(expr: Tree): StmtGoto = {
    if (expr eq tree.expr) {
      tree
    } else {
      StmtGoto(expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: StmtAssign)(lhs: Tree, rhs: Tree): StmtAssign = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      StmtAssign(lhs.asInstanceOf[Expr], rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: StmtUpdate)(lhs: Tree, rhs: Tree): StmtUpdate = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      StmtUpdate(lhs.asInstanceOf[Expr], tree.op, rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: StmtPost)(expr: Tree): StmtPost = {
    if (expr eq tree.expr) {
      tree
    } else {
      StmtPost(expr.asInstanceOf[Expr], tree.op) withLoc tree.loc
    }
  }

  def apply(tree: StmtDelayed)(lhs: Tree, rhs: Tree): StmtDelayed = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      StmtDelayed(lhs.asInstanceOf[Expr], rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: StmtOutcall)(output: Tree, func: Tree, inputs: List[Tree]): StmtOutcall = {
    if ((output eq tree.output) && (func eq tree.func) && (output eq tree.output)) {
      tree
    } else {
      assert(inputs forall { _.isInstanceOf[Expr] })
      StmtOutcall(
        output.asInstanceOf[Expr],
        func.asInstanceOf[Expr],
        inputs.asInstanceOf[List[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: StmtExpr)(expr: Tree): StmtExpr = {
    if (expr eq tree.expr) {
      tree
    } else {
      StmtExpr(expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: StmtStall)(cond: Tree): StmtStall = {
    if (cond eq tree.cond) {
      tree
    } else {
      StmtStall(cond.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: StmtAssertion)(assertion: Tree): StmtAssertion = {
    if (assertion eq tree.assertion) {
      tree
    } else {
      StmtAssertion(assertion.asInstanceOf[Assertion]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Case
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: CaseRegular)(cond: List[Tree], stmts: List[Tree]): CaseRegular = {
    if ((cond eq tree.cond) && (stmts eq tree.stmts)) {
      tree
    } else {
      assert(cond forall { _.isInstanceOf[Expr] })
      assert(stmts forall { _.isInstanceOf[Stmt] })
      CaseRegular(cond.asInstanceOf[List[Expr]], stmts.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: CaseDefault)(stmts: List[Tree]): CaseDefault = {
    if (stmts eq tree.stmts) {
      tree
    } else {
      assert(stmts forall { _.isInstanceOf[Stmt] })
      CaseDefault(stmts.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: CaseGen)(gen: Tree): CaseGen = {
    if (gen eq tree.gen) {
      tree
    } else {
      CaseGen(gen.asInstanceOf[Gen]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Expr
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: ExprCall)(expr: Tree, args: List[Tree]): ExprCall = {
    if ((expr eq tree.expr) && (args eq tree.args)) {
      tree
    } else {
      assert(args forall { _.isInstanceOf[Arg] })
      ExprCall(expr.asInstanceOf[Expr], args.asInstanceOf[List[Arg]]) withLoc tree.loc
    }
  }

  def apply(tree: ExprUnary)(expr: Tree): ExprUnary = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ExprBinary)(lhs: Tree, rhs: Tree): ExprBinary = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      tree.copy(lhs = lhs.asInstanceOf[Expr], rhs = rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ExprTernary)(cond: Tree, thenExpr: Tree, elseExpr: Tree): ExprTernary = {
    if ((cond eq tree.cond) && (thenExpr eq tree.thenExpr) && (elseExpr eq tree.elseExpr)) {
      tree
    } else {
      ExprTernary(cond.asInstanceOf[Expr], thenExpr.asInstanceOf[Expr], elseExpr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ExprRep)(count: Tree, expr: Tree): ExprRep = {
    if ((count eq tree.count) && (expr eq tree.expr)) {
      tree
    } else {
      ExprRep(count.asInstanceOf[Expr], expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ExprCat)(parts: List[Tree]): ExprCat = {
    if (parts eq tree.parts) {
      tree
    } else {
      assert(parts forall { _.isInstanceOf[Expr] })
      ExprCat(parts.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  def apply(tree: ExprIndex)(expr: Tree, index: Tree): ExprIndex = {
    if ((expr eq tree.expr) && (index eq tree.index)) {
      tree
    } else {
      ExprIndex(expr.asInstanceOf[Expr], index.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ExprSlice)(expr: Tree, lIdx: Tree, rIdx: Tree): ExprSlice = {
    if ((expr eq tree.expr) && (lIdx eq tree.lIdx) && (rIdx eq tree.rIdx)) {
      tree
    } else {
      tree.copy(
        expr = expr.asInstanceOf[Expr],
        lIdx = lIdx.asInstanceOf[Expr],
        rIdx = rIdx.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: ExprSelect)(expr: Tree, idxs: List[Tree]): ExprSelect = {
    if ((expr eq tree.expr) && (idxs eq tree.idxs)) {
      tree
    } else {
      assert(idxs forall { _.isInstanceOf[Expr] })
      tree.copy(
        expr = expr.asInstanceOf[Expr],
        idxs = idxs.asInstanceOf[List[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: ExprRef)(ref: Tree): ExprRef = {
    if (ref eq tree.ref) {
      tree
    } else {
      ExprRef(ref.asInstanceOf[Ref]) withLoc tree.loc
    }
  }

  def apply(tree: ExprCast)(expr: Tree): ExprCast = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Arg
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: ArgP)(expr: Tree): ArgP = {
    if (expr eq tree.expr) {
      tree
    } else {
      ArgP(expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ArgN)(expr: Tree): ArgN = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Thicket
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: Thicket)(trees: List[Tree]): Thicket = {
    if (trees eq tree.trees) {
      tree
    } else {
      Thicket(trees)
    }
  }

}
