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
// Dereference all TypeRef instances in Type
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.core.TypeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.ast.Trees._

final class ChaseTypeRefs(implicit cc: CompilerContext) extends TypeTransformer {

  override def transform(kind: Type): Type = kind match {
    case TypeRef(Sym(symbol)) => symbol.denot.kind rewrite this
    case TypeRef(other)       => cc.ice(other, s"ChaseTypeRefs applied to '${kind}'")
    case _                    => kind
  }

  override def finalCheck(kind: Type) = {
    kind visit {
      case node: TypeRef => {
        cc.ice(node.ref, s"ChaseTypeRefs should have removed all TypeRef nodes, but '${node}' remains")
      }
    }
  }
}
