////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// TreesLike implemented as Product of direct or indirect descendants
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

// Similarly to ProductTree, StructuredTree is a TreeLike that is a Product, but
// children can be held indirectly through members that are containers.
// Children can be held either:
// - Directly as a member of type TreeLike
// - Through a member of type Iterable[_]
// - Through a member of type Option[_]
trait StructuredTree extends TreeLike with Product {

  def children: Iterator[TreeLike] = {
    val iterators = productIterator collect {
      case tree: TreeLike       => Iterator.single(tree)
      case trees: Iterable[_]   => trees.iterator collect { case tree: TreeLike => tree }
      case Some(tree: TreeLike) => Iterator.single(tree)
    }
    iterators.flatten
  }

  override def productPrefix: String = "StructuredTree"

}
