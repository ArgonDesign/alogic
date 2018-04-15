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
// A Type transformer used to modify Types
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.lib.TreeLikeTransformer

import Types._

// Type transformers are applied during a post-order traversal of a Type.
abstract class TypeTransformer(implicit val cc: CompilerContext) extends TreeLikeTransformer[Type] {

  ///////////////////////////////////////////////////////////////////////////////
  // Internals
  ///////////////////////////////////////////////////////////////////////////////

  protected final override def walk(tree: Type): Type = {
    enter(tree)
    tree match {
      case TypeCombStmt   => transform(TypeCombStmt)
      case TypeCtrlStmt   => transform(TypeCtrlStmt)
      case TypeState      => transform(TypeState)
      case node: TypeSInt => transform(node)
      case node: TypeUInt => transform(node)
      case node: TypeNum  => transform(node)
      case node: TypeVector => {
        val elementType = walk(node.elementType)
        transform(TypeCopier(node)(elementType))
      }
      case node: TypeArray => {
        val elementType = walk(node.elementType)
        transform(TypeCopier(node)(elementType))
      }
      case node: TypeStack => {
        val elementType = walk(node.elementType)
        transform(TypeCopier(node)(elementType))
      }
      case node: TypeStruct => {
        val fieldTypes = walk(node.fieldTypes)
        transform(TypeCopier(node)(fieldTypes))
      }
      case TypeVoid      => transform(TypeVoid)
      case node: TypeRef => transform(node)
      case node: TypeCombFunc => {
        val argTypes = walk(node.argTypes)
        val retType = walk(node.retType)
        transform(TypeCopier(node)(argTypes, retType))
      }
      case node: TypeCtrlFunc => {
        val argTypes = walk(node.argTypes)
        val retType = walk(node.retType)
        transform(TypeCopier(node)(argTypes, retType))
      }
      case node: TypeEntity => transform(node)
      case TypeStr          => transform(TypeStr)
      case node: TypeIn => {
        val kind = walk(node.kind)
        transform(TypeCopier(node)(kind))
      }
      case node: TypeOut => {
        val kind = walk(node.kind)
        transform(TypeCopier(node)(kind))
      }
      case node: TypePipeline => {
        val kind = walk(node.kind)
        transform(TypeCopier(node)(kind))
      }
      case node: TypeParam => {
        val kind = walk(node.kind)
        transform(TypeCopier(node)(kind))
      }
      case node: TypeConst => {
        val kind = walk(node.kind)
        transform(TypeCopier(node)(kind))
      }
      case node: TypeType => {
        val kind = walk(node.kind)
        transform(TypeCopier(node)(kind))
      }
      case TypeMisc           => transform(TypeMisc)
      case node: TypePolyFunc => transform(node)
      case TypeError          => transform(TypeError)
    }
  }
}
