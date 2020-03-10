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
// A Tree transformer used to modify Trees
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

abstract class StatelessTreeTransformer(implicit cc: CompilerContext) extends TreeTransformer {

  //////////////////////////////////////////////////////////////////////////////
  // Transform specific interface to be filled in by sub-classes
  //////////////////////////////////////////////////////////////////////////////

  // 'skip' is a predicate that can be used to mark subtrees that should not be
  // visited. If 'skip' returns true for a node, that node will not be visited,
  // i.e.: enter and transform will not be called on that node, or any of their
  // children, leaving the subtree unmodified
  protected def skip(tree: Tree): Boolean = false

  // 'enter' is called on each non-skipped node, in pre-order (before visiting
  // any of their children) when the node is first encountered. 'enter' is used
  // to modify the state of the TreeTransformer or the context before
  // transforming children. When 'enter' returns Some(node), the node 'enter'
  // is called with is immediately replaced with the returned node, without
  // visiting the children or calling transform on the node, effectively
  // preempting traversal of the subtree below the given tree. If 'enter'
  // returns None, traversal continues by transforming all children and the
  // entered node.
  protected def enter(tree: Tree): Option[Tree] = None

  // 'transform' is called on each non-skipped node, in post-order (after all
  // children have been transformed). 'transform' us used to modify tree nodes.
  // When 'transform' is called, all child nodes have already been transformed
  // using the same function.
  protected def transform(tree: Tree): Tree = tree

  //////////////////////////////////////////////////////////////////////////////
  // Implementation of walkTree
  //////////////////////////////////////////////////////////////////////////////

  // Walk single node
  final def walkTree(tree: Tree): Tree =
    if (skip(tree)) {
      tree
    } else {
      // Call enter in pre order
      enter(tree) match {
        // 'enter' provided replacement, use it
        case Some(replacement) => replacement
        // 'enter' did not provide replacement, traverse the tree.
        case None =>
          // Walk children
          val walked = walkChildren(tree)
          // Apply transform
          walked match {
            case Thicket(ts) => Thicket(transform(ts))
            case Stump       => Stump
            case other       => transform(other)
          }
      }
    }

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  private final def transform(trees: List[Tree]): List[Tree] = trees flatMap { t =>
    transform(t) match {
      case Thicket(results) => results
      case result           => List(result)
    }
  }
}
