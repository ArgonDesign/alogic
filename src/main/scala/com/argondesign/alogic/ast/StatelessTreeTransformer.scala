////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Stateless Tree transformer that walks and transforms according to 'skip',
//  'enter' and 'transform'.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

abstract class StatelessTreeTransformer(implicit cc: CompilerContext) extends TreeTransformer(cc) {

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

}
