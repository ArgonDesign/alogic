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
// A copy on write Type copier used in type transformations
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import Types._

// scalastyle:off token

// Given a Type and new child nodes, create new Type if children are not the same as
// the children of the current Type, otherwise reuse existing Type. Note that only
// fields containing Type instances are checked as other fields are not transformed
// recursively in TypeTransformer
object TypeCopier {

  def apply(tree: TypeVector)(elementType: Type): TypeVector = {
    if (elementType eq tree.elementType) tree else TypeVector(elementType, tree.size)
  }

  def apply(tree: TypeArray)(elementType: Type): TypeArray = {
    if (elementType eq tree.elementType) tree else TypeArray(elementType, tree.size)
  }

  def apply(tree: TypeStruct)(fieldTypes: List[Type]): TypeStruct = {
    if (fieldTypes eq tree.fieldTypes) {
      tree
    } else {
      TypeStruct(tree.fieldNames, fieldTypes)
    }
  }

  def apply(tree: TypeCombFunc)(argTypes: List[Type], retType: Type): TypeCombFunc = {
    if ((argTypes eq tree.argTypes) && (retType eq tree.retType)) {
      tree
    } else {
      TypeCombFunc(argTypes, retType);
    }
  }

  def apply(tree: TypeCtrlFunc)(argTypes: List[Type], retType: Type): TypeCtrlFunc = {
    if ((argTypes eq tree.argTypes) && (retType eq tree.retType)) {
      tree
    } else {
      TypeCtrlFunc(argTypes, retType);
    }
  }

  def apply(tree: TypeEntity)(portTypes: List[Type], paramTypes: List[Type]): TypeEntity = {
    if ((portTypes eq tree.portTypes) && (paramTypes eq tree.paramTypes)) {
      tree
    } else {
      TypeEntity(tree.portNames, portTypes, tree.paramNames, paramTypes)
    }
  }

  def apply(tree: TypeIn)(kind: Type): TypeIn = {
    if (kind eq tree.kind) tree else TypeIn(kind, tree.fct)
  }

  def apply(tree: TypeOut)(kind: Type): TypeOut = {
    if (kind eq tree.kind) tree else TypeOut(kind, tree.fct, tree.st)
  }

  def apply(tree: TypePipeline)(kind: Type): TypePipeline = {
    if (kind eq tree.kind) tree else TypePipeline(kind)
  }

  def apply(tree: TypeParam)(kind: Type): TypeParam = {
    if (kind eq tree.kind) tree else TypeParam(kind)
  }

  def apply(tree: TypeConst)(kind: Type): TypeConst = {
    if (kind eq tree.kind) tree else TypeConst(kind)
  }

  def apply(tree: TypeType)(kind: Type): TypeType = {
    if (kind eq tree.kind) tree else TypeType(kind)
  }

}
