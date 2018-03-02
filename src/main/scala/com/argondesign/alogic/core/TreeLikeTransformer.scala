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
// Base class of transformers of TreeLike structures
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

// Tree transformers are applied during a post-order traversal of a Tree.
abstract class TreeLikeTransformer[T <: TreeLike] {

  // enter is called when entering a node, before visiting any children.
  // enter is used to modify the state of the tree transformer or the context
  // before transforming children.
  protected def enter(tree: T): Unit = ()

  // transform is called after all children have already been visited and transformed.
  protected def transform(tree: T): T = tree

  // finalCheck is invoked with the root of the transformed tree.
  // This can be used to verify invariants introduced by this transform
  protected def finalCheck(tree: T): Unit = ()

  ///////////////////////////////////////////////////////////////////////////////
  // Public API
  ///////////////////////////////////////////////////////////////////////////////

  def apply(tree: T): T = {
    // Walk the tree
    val result = walk(tree)
    // Apply final check
    finalCheck(result)
    // Yield result
    result
  }

  ///////////////////////////////////////////////////////////////////////////////
  // Internals
  ///////////////////////////////////////////////////////////////////////////////

  // Walk list, but return the original list if nothing is transformed
  final protected def walk(trees: List[T]): List[T] = {
    trees match {
      case head :: tail => {
        val newHead = walk(head)
        val newTail = walk(tail)
        if ((head eq newHead) && (tail eq newTail)) trees else newHead :: newTail
      }
      case Nil => Nil
    }
  }

  // Walk option,but return the original option if value is not transformed
  final protected def walk(treeOpt: Option[T]): Option[T] = {
    treeOpt match {
      case Some(tree) => {
        val newTree = walk(tree)
        if (newTree eq tree) treeOpt else Some(newTree)
      }
      case None => treeOpt
    }
  }

  // Walk single node
  protected def walk(tree: T): T
}
