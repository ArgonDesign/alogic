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
  // Ref
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: Ident)(indices: List[Tree]): Ident = {
    if (indices eq tree.idxs) {
      tree
    } else {
      assert(indices forall {
        _.isInstanceOf[Expr]
      })
      Ident(tree.base, indices.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Desc
  //////////////////////////////////////////////////////////////////////////////

  def apply(
      tree: DescVar
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      initOpt: Option[Tree]
    ): DescVar = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (initOpt eq tree.initOpt)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
      DescVar(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr],
        initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescVal
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      init: Tree
    ): DescVal = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (init eq tree.init)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescVal(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr],
        init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescStatic
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      initOpt: Option[Tree]
    ): DescStatic = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (initOpt eq tree.initOpt)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
      DescStatic(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr],
        initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescIn)(ref: Tree, attr: List[Tree], spec: Tree): DescIn = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescOut
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      initOpt: Option[Tree]
    ): DescOut = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (initOpt eq tree.initOpt)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        spec = spec.asInstanceOf[Expr],
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescPipeVar)(ref: Tree, attr: List[Tree], spec: Tree): DescPipeVar = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescPipeVar(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescPipeIn)(ref: Tree, attr: List[Tree]): DescPipeIn = {
    if ((ref eq tree.ref) && (attr eq tree.attr)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescPipeOut)(ref: Tree, attr: List[Tree]): DescPipeOut = {
    if ((ref eq tree.ref) && (attr eq tree.attr)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescParam
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      initOpt: Option[Tree]
    ): DescParam = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (initOpt eq tree.initOpt)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        spec = spec.asInstanceOf[Expr],
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescParamType
    )(
      ref: Tree,
      attr: List[Tree],
      initOpt: Option[Tree]
    ): DescParamType = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (initOpt eq tree.initOpt)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescConst
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      init: Tree
    ): DescConst = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (init eq tree.init)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescConst(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr],
        init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescArray
    )(
      ref: Tree,
      attr: List[Tree],
      elem: Tree,
      size: Tree
    ): DescArray = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescArray(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        elem.asInstanceOf[Expr],
        size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescSram
    )(
      ref: Tree,
      attr: List[Tree],
      elem: Tree,
      size: Tree
    ): DescSram = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (elem eq tree.elem) && (size eq tree.size)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        elem = elem.asInstanceOf[Expr],
        size = size.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescType)(ref: Tree, attr: List[Tree], spec: Tree): DescType = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec)) {
      tree
    } else {
      DescType(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescEntity)(ref: Tree, attr: List[Tree], body: List[Tree]): DescEntity = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (body eq tree.body)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(body forall {
        _.isInstanceOf[Ent]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescRecord)(ref: Tree, attr: List[Tree], body: List[Tree]): DescRecord = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (body eq tree.body)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(body forall {
        _.isInstanceOf[Rec]
      })
      DescRecord(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        body.asInstanceOf[List[Rec]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescInstance)(ref: Tree, attr: List[Tree], spec: Tree): DescInstance = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescInstance(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescSingleton)(ref: Tree, attr: List[Tree], body: List[Tree]): DescSingleton = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (body eq tree.body)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(body forall {
        _.isInstanceOf[Ent]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescFunc
    )(
      ref: Tree,
      attr: List[Tree],
      ret: Tree,
      args: List[Tree],
      body: List[Tree]
    ): DescFunc = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (ret eq tree.ret) && (args eq tree.args) && (body eq tree.body)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(args forall {
        _.isInstanceOf[Desc]
      })
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        ret = ret.asInstanceOf[Expr],
        args = args.asInstanceOf[List[Desc]],
        body = body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescPackage)(ref: Tree, attr: List[Tree], body: List[Tree]): DescPackage = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (body eq tree.body)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(body forall {
        _.isInstanceOf[Pkg]
      })
      DescPackage(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        body.asInstanceOf[List[Pkg]]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescGenVar
    )(
      ref: Tree,
      attr: List[Tree],
      spec: Tree,
      init: Tree
    ): DescGenVar = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (spec eq tree.spec) && (init eq tree.init)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescGenVar(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        spec.asInstanceOf[Expr],
        init.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescGenIf
    )(
      ref: Tree,
      attr: List[Tree],
      cases: List[Tree],
      defaults: List[Tree]
    ): DescGenIf = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (cases eq tree.cases) && (defaults eq tree.defaults)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      cases forall {
        _.isInstanceOf[GenCase]
      }
      DescGenIf(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        cases.asInstanceOf[List[GenCase]],
        defaults
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescGenFor
    )(
      ref: Tree,
      attr: List[Tree],
      inits: List[Tree],
      cond: Tree,
      steps: List[Tree],
      body: List[Tree]
    ): DescGenFor = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (inits eq tree.inits) && (cond eq tree.cond) && (steps eq tree.steps) && (body eq tree.body)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      assert(inits forall {
        _.isInstanceOf[Desc]
      })
      assert(steps forall {
        _.isInstanceOf[Stmt]
      })
      DescGenFor(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        inits.asInstanceOf[List[Desc]],
        cond.asInstanceOf[Expr],
        steps.asInstanceOf[List[Stmt]],
        body
      ) withLoc tree.loc
    }
  }

  def apply(
      tree: DescGenRange
    )(
      ref: Tree,
      attr: List[Tree],
      init: Tree,
      end: Tree,
      body: List[Tree]
    ): DescGenRange = {
    if (
      (ref eq tree.ref) && (attr eq tree.attr) && (init eq tree.init) && (end eq tree.end) && (body eq tree.body)
    ) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        init = init.asInstanceOf[Desc],
        end = end.asInstanceOf[Expr],
        body = body
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescGenScope)(ref: Tree, attr: List[Tree], body: List[Tree]): DescGenScope = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (body eq tree.body)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      DescGenScope(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        body
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescAlias)(ref: Tree, attr: List[Tree], expr: Tree): DescAlias = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (expr eq tree.expr)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref = ref.asInstanceOf[Ref],
        attr = attr.asInstanceOf[List[Attr]],
        expr = expr.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DescParametrized)(ref: Tree, attr: List[Tree], desc: Tree): DescParametrized = {
    if ((ref eq tree.ref) && (attr eq tree.attr) && (desc eq tree.desc)) {
      tree
    } else {
      assert(attr forall {
        _.isInstanceOf[Attr]
      })
      tree.copy(
        ref.asInstanceOf[Ref],
        attr.asInstanceOf[List[Attr]],
        desc.asInstanceOf[Desc]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Attr
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: AttrExpr)(expr: Tree): AttrExpr = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // GenCase
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: GenCase)(cond: Tree, body: List[Tree]): GenCase = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      GenCase(cond.asInstanceOf[Expr], body) withLoc tree.loc
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
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclVal)(spec: Tree): DeclVal = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclStatic)(spec: Tree): DeclStatic = {
    if (spec eq tree.spec) {
      tree
    } else {
      tree.copy(
        spec = spec.asInstanceOf[Expr]
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

  def apply(tree: DeclPipeVar)(spec: Tree): DeclPipeVar = {
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

  def apply(tree: DeclArray)(elem: Tree): DeclArray = {
    if (elem eq tree.elem) {
      tree
    } else {
      tree.copy(
        elem = elem.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclSram)(elem: Tree): DeclSram = {
    if (elem eq tree.elem) {
      tree
    } else {
      tree.copy(
        elem = elem.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclStack)(elem: Tree): DeclStack = {
    if (elem eq tree.elem) {
      tree
    } else {
      tree.copy(
        elem = elem.asInstanceOf[Expr]
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
      assert(decls forall {
        _.isInstanceOf[Decl]
      })
      tree.copy(
        decls = decls.asInstanceOf[List[Decl]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclRecord)(decls: List[Tree]): DeclRecord = {
    if (decls eq tree.decls) {
      tree
    } else {
      assert(decls forall {
        _.isInstanceOf[Decl]
      })
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
      assert(decls forall {
        _.isInstanceOf[Decl]
      })
      tree.copy(
        decls = decls.asInstanceOf[List[Decl]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DeclFunc)(ret: Tree, args: List[Tree]): DeclFunc = {
    if ((ret eq tree.ret) && (args eq tree.args)) {
      tree
    } else {
      assert(args forall {
        _.isInstanceOf[Decl]
      })
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
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
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

  def apply(tree: DefnStatic)(initOpt: Option[Tree]): DefnStatic = {
    if (initOpt eq tree.initOpt) {
      tree
    } else {
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(
        initOpt = initOpt.asInstanceOf[Option[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnOut)(initOpt: Option[Tree]): DefnOut = {
    if (initOpt eq tree.initOpt) {
      tree
    } else {
      assert(initOpt forall {
        _.isInstanceOf[Expr]
      })
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

  def apply(tree: DefnEntity)(body: List[Tree]): DefnEntity = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Ent]
      })
      tree.copy(
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnRecord)(body: List[Tree]): DefnRecord = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Rec]
      })
      tree.copy(
        body = body.asInstanceOf[List[Rec]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnSingleton)(body: List[Tree]): DefnSingleton = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Ent]
      })
      tree.copy(
        body = body.asInstanceOf[List[Ent]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnFunc)(args: List[Tree], body: List[Tree]): DefnFunc = {
    if ((args eq tree.args) && (body eq tree.body)) {
      tree
    } else {
      assert(args forall {
        _.isInstanceOf[Defn]
      })
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
      tree.copy(
        args = args.asInstanceOf[List[Defn]],
        body = body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: DefnState)(body: List[Tree]): DefnState = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
      tree.copy(
        body = body.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Import
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: ImportOne)(expr: Tree, identOpt: Option[Tree]): ImportOne = {
    if ((expr eq tree.expr) && (identOpt eq tree.identOpt)) {
      tree
    } else {
      assert(identOpt forall { _.isInstanceOf[Ident] })
      tree.copy(
        expr = expr.asInstanceOf[Expr],
        identOpt = identOpt.asInstanceOf[Option[Ident]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Using
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: UsingOne)(expr: Tree, identOpt: Option[Tree]): UsingOne = {
    if ((expr eq tree.expr) && (identOpt eq tree.identOpt)) {
      tree
    } else {
      assert(identOpt forall {
        _.isInstanceOf[Ident]
      })
      UsingOne(
        expr.asInstanceOf[Expr],
        identOpt.asInstanceOf[Option[Ident]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: UsingAll)(expr: Tree): UsingAll = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: UsingGenLoopBody)(expr: Tree): UsingGenLoopBody = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // From
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: FromOne)(expr: Tree, name: Tree, identOpt: Option[Tree]): FromOne = {
    if ((expr eq tree.expr) && (name eq tree.name) && (identOpt eq tree.identOpt)) {
      tree
    } else {
      assert(identOpt forall { _.isInstanceOf[Ident] })
      tree.copy(
        expr = expr.asInstanceOf[Expr],
        name = name.asInstanceOf[Expr],
        identOpt = identOpt.asInstanceOf[Option[Ident]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: FromAll)(expr: Tree): FromAll = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(expr = expr.asInstanceOf[Expr]) withLoc tree.loc
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
  // Pkg
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: PkgSplice)(spliceable: Tree): PkgSplice = {
    if (spliceable eq tree.tree) {
      tree
    } else {
      PkgSplice(spliceable.asInstanceOf[Spliceable]) withLoc tree.loc
    }
  }

  def apply(tree: PkgCompile)(expr: Tree, identOpt: Option[Tree]): PkgCompile = {
    if ((expr eq tree.expr) && (identOpt eq tree.identOpt)) {
      tree
    } else {
      assert(identOpt forall { _.isInstanceOf[Ident] })
      PkgCompile(
        expr.asInstanceOf[Expr],
        identOpt.asInstanceOf[Option[Ident]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Ent
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: EntSplice)(spliceable: Tree): EntSplice = {
    if (spliceable eq tree.tree) {
      tree
    } else {
      EntSplice(spliceable.asInstanceOf[Spliceable]) withLoc tree.loc
    }
  }

  def apply(tree: EntConnect)(lhs: Tree, rhs: List[Tree]): EntConnect = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      assert(rhs forall {
        _.isInstanceOf[Expr]
      })
      EntConnect(lhs.asInstanceOf[Expr], rhs.asInstanceOf[List[Expr]]) withLoc tree.loc
    }
  }

  def apply(tree: EntAssign)(lhs: Tree, rhs: Tree): EntAssign = {
    if ((lhs eq tree.lhs) && (rhs eq tree.rhs)) {
      tree
    } else {
      EntAssign(lhs.asInstanceOf[Expr], rhs.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: EntCombProcess)(stmts: List[Tree]): EntCombProcess = {
    if (stmts eq tree.stmts) {
      tree
    } else {
      assert(stmts forall {
        _.isInstanceOf[Stmt]
      })
      EntCombProcess(stmts.asInstanceOf[List[StmtIf]]) withLoc tree.loc
    }
  }

  def apply(
      tree: EntClockedProcess
    )(
      clk: Tree,
      rstOpt: Option[Tree],
      stmts: List[Tree]
    ): EntClockedProcess = {
    if ((clk eq tree.clk) && (rstOpt eq tree.rstOpt) && (stmts eq tree.stmts)) {
      tree
    } else {
      assert(rstOpt forall {
        _.isInstanceOf[Expr]
      })
      assert(stmts forall {
        _.isInstanceOf[Stmt]
      })
      EntClockedProcess(
        clk.asInstanceOf[Expr],
        rstOpt.asInstanceOf[Option[Expr]],
        stmts.asInstanceOf[List[Stmt]]
      ) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Rec
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: RecSplice)(spliceable: Tree): RecSplice = {
    if (spliceable eq tree.tree) {
      tree
    } else {
      RecSplice(spliceable.asInstanceOf[Spliceable]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Stmt
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: StmtSplice)(spliceable: Tree): StmtSplice = {
    if (spliceable eq tree.tree) {
      tree
    } else {
      StmtSplice(spliceable.asInstanceOf[Spliceable]) withLoc tree.loc
    }
  }

  def apply(tree: StmtBlock)(body: List[Tree]): StmtBlock = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
      StmtBlock(body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(
      tree: StmtIf
    )(
      cond: Tree,
      thenStmts: List[Tree],
      elseStmts: List[Tree]
    ): StmtIf = {
    if ((cond eq tree.cond) && (thenStmts eq tree.thenStmts) && (elseStmts eq tree.elseStmts)) {
      tree
    } else {
      assert(thenStmts forall {
        _.isInstanceOf[Stmt]
      })
      assert(elseStmts forall {
        _.isInstanceOf[Stmt]
      })
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
      assert(cases forall {
        _.isInstanceOf[Case]
      })
      StmtCase(
        expr.asInstanceOf[Expr],
        cases.asInstanceOf[List[Case]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: StmtLoop)(body: List[Tree]): StmtLoop = {
    if (body eq tree.body) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
      StmtLoop(body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: StmtWhile)(cond: Tree, body: List[Tree]): StmtWhile = {
    if ((cond eq tree.cond) && (body eq tree.body)) {
      tree
    } else {
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
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
    if (
      (inits eq tree.inits) && (cond eq tree.condOpt) && (steps eq tree.steps) && (body eq tree.body)
    ) {
      tree
    } else {
      assert(inits forall {
        _.isInstanceOf[Stmt]
      })
      assert(cond forall {
        _.isInstanceOf[Expr]
      })
      assert(steps forall {
        _.isInstanceOf[Stmt]
      })
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
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
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
      StmtDo(cond.asInstanceOf[Expr], body.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: StmtLet)(inits: List[Tree], body: List[Tree]): StmtLet = {
    if ((inits eq tree.inits) && (body eq tree.body)) {
      tree
    } else {
      assert(inits forall {
        _.isInstanceOf[Stmt]
      })
      assert(body forall {
        _.isInstanceOf[Stmt]
      })
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

  def apply(tree: StmtReturn)(exprOpt: Option[Tree]): StmtReturn = {
    if (exprOpt eq tree.exprOpt) {
      tree
    } else {
      assert(exprOpt forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(exprOpt = exprOpt.asInstanceOf[Option[Expr]]) withLoc tree.loc
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
    if ((output eq tree.output) && (func eq tree.func) && (inputs eq tree.inputs)) {
      tree
    } else {
      assert(inputs forall {
        _.isInstanceOf[Expr]
      })
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

  def apply(tree: StmtWait)(cond: Tree): StmtWait = {
    if (cond eq tree.cond) {
      tree
    } else {
      StmtWait(cond.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Case
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: CaseSplice)(spliceable: Tree): CaseSplice = {
    if (spliceable eq tree.tree) {
      tree
    } else {
      CaseSplice(spliceable.asInstanceOf[Spliceable]) withLoc tree.loc
    }
  }

  def apply(tree: CaseRegular)(cond: List[Tree], stmts: List[Tree]): CaseRegular = {
    if ((cond eq tree.cond) && (stmts eq tree.stmts)) {
      tree
    } else {
      assert(cond forall {
        _.isInstanceOf[Expr]
      })
      assert(stmts forall {
        _.isInstanceOf[Stmt]
      })
      CaseRegular(cond.asInstanceOf[List[Expr]], stmts.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  def apply(tree: CaseDefault)(stmts: List[Tree]): CaseDefault = {
    if (stmts eq tree.stmts) {
      tree
    } else {
      assert(stmts forall {
        _.isInstanceOf[Stmt]
      })
      CaseDefault(stmts.asInstanceOf[List[Stmt]]) withLoc tree.loc
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Expr
  //////////////////////////////////////////////////////////////////////////////

  def apply(tree: ExprCall)(expr: Tree, args: List[Tree]): ExprCall = {
    if ((expr eq tree.expr) && (args eq tree.args)) {
      tree
    } else {
      assert(args forall {
        _.isInstanceOf[Arg]
      })
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

  def apply(tree: ExprCond)(cond: Tree, thenExpr: Tree, elseExpr: Tree): ExprCond = {
    if ((cond eq tree.cond) && (thenExpr eq tree.thenExpr) && (elseExpr eq tree.elseExpr)) {
      tree
    } else {
      ExprCond(
        cond.asInstanceOf[Expr],
        thenExpr.asInstanceOf[Expr],
        elseExpr.asInstanceOf[Expr]
      ) withLoc tree.loc
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
      assert(parts forall {
        _.isInstanceOf[Expr]
      })
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

  def apply(tree: ExprDot)(expr: Tree, idxs: List[Tree]): ExprDot = {
    if ((expr eq tree.expr) && (idxs eq tree.idxs)) {
      tree
    } else {
      assert(idxs forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(
        expr = expr.asInstanceOf[Expr],
        idxs = idxs.asInstanceOf[List[Expr]]
      ) withLoc tree.loc
    }
  }

  def apply(tree: ExprSel)(expr: Tree): ExprSel = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(
        expr = expr.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: ExprSymSel)(expr: Tree): ExprSymSel = {
    if (expr eq tree.expr) {
      tree
    } else {
      tree.copy(
        expr = expr.asInstanceOf[Expr]
      ) withLoc tree.loc
    }
  }

  def apply(tree: ExprIdent)(ident: Tree): ExprIdent = {
    if (ident eq tree.ident) {
      tree
    } else {
      ExprIdent(ident.asInstanceOf[Ident]) withLoc tree.loc
    }
  }

  def apply(tree: ExprOld)(expr: Tree): ExprOld = {
    if (expr eq tree.expr) {
      tree
    } else {
      ExprOld(expr.asInstanceOf[Expr]) withLoc tree.loc
    }
  }

  def apply(tree: ExprThis)(expr: Tree): ExprThis = {
    if (expr eq tree.expr) {
      tree
    } else {
      ExprThis(expr.asInstanceOf[Expr]) withLoc tree.loc
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

  def apply(tree: ArgD)(idxs: List[Tree], expr: Tree): ArgD = {
    if ((idxs eq tree.idxs) && (expr eq tree.expr)) {
      tree
    } else {
      assert(idxs forall {
        _.isInstanceOf[Expr]
      })
      tree.copy(
        idxs = idxs.asInstanceOf[List[Expr]],
        expr = expr.asInstanceOf[Expr]
      ) withLoc tree.loc
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
