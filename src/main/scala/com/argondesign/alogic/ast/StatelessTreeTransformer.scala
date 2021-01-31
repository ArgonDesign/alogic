////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Stateless Tree transformer that walks and transforms according to 'skip',
//  'enter' and 'transform'.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

abstract class StatelessTreeTransformer extends TreeTransformer {

  //////////////////////////////////////////////////////////////////////////////
  // Implementation of walkTree
  //////////////////////////////////////////////////////////////////////////////

  // Walk single node
  final def walkTree(tree: Tree): Tree =
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
