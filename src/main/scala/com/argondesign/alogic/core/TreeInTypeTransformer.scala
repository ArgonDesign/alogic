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
// A TypeTransformer that transforms all Tree instances within a Type tree,
// using the given TreeTransformer. Note that transformation is done directly
// with 'walk', so in particular, finalChecks are not applied.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._

abstract class TreeInTypeTransformer(treeTransformer: TreeTransformer)(implicit cc: CompilerContext)
    extends TypeTransformer {

  override def transform(kind: Type) = kind match {
    case node @ TypeIdent(ident) => {
      val newIdent = treeTransformer.walk(ident).asInstanceOf[Ident]
      if (newIdent eq ident) node else TypeIdent(newIdent)
    }
    case node @ TypeSInt(size) => {
      val newSize = treeTransformer.walk(size).asInstanceOf[Expr]
      if (newSize eq size) node else TypeSInt(newSize)
    }
    case node @ TypeUInt(size) => {
      val newSize = treeTransformer.walk(size).asInstanceOf[Expr]
      if (newSize eq size) node else TypeUInt(newSize)
    }
    case node @ TypeVector(_, size) => {
      val newSize = treeTransformer.walk(size).asInstanceOf[Expr]
      if (newSize eq size) node else node.copy(size = newSize)
    }
    case node @ TypeArray(_, size) => {
      val newSize = treeTransformer.walk(size).asInstanceOf[Expr]
      if (newSize eq size) node else node.copy(size = newSize)
    }
    case node @ TypeStack(_, size) => {
      val newSize = treeTransformer.walk(size).asInstanceOf[Expr]
      if (newSize eq size) node else node.copy(size = newSize)
    }
    case node @ TypeSram(_, size, _) => {
      val newSize = treeTransformer.walk(size).asInstanceOf[Expr]
      if (newSize eq size) node else node.copy(size = newSize)
    }
    case _ => kind
  }
}
