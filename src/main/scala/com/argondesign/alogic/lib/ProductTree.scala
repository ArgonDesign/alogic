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
// TreesLike implemented as Product of direct descendants
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

// ProductTree is a TreeLike that is a Product, holding children as elements.
// Note that there can be elements that are of other type present in the Product.
// These are not considered children.
trait ProductTree extends TreeLike with Product {

  def children: Iterator[TreeLike] = {
    productIterator collect {
      case tree: TreeLike => tree
    }
  }

  override def productPrefix: String = "ProductTree"

}
