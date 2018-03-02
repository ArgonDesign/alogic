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
      case node: TypeInt => transform(node)
      case node: TypeVector => {
        val elementType = walk(node.elementType)
        transform(TypeCopier(node)(elementType))
      }
      case node: TypeArray => {
        val elementType = walk(node.elementType)
        transform(TypeCopier(node)(elementType))
      }
      case node: TypeStruct => {
        val fieldTypes = walk(node.fieldTypes)
        transform(TypeCopier(node)(fieldTypes))
      }
      case TypeVoid      => transform(TypeVoid)
      case node: TypeRef => transform(node)
      case TypeFunc      => transform(TypeFunc)
      case TypeEntity    => transform(TypeEntity)
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
    }
  }
}
