////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Replace TypeRef instances with the type of the reference symbol
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeTransformer
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeRef

final class DereferenceTypeRefs(implicit cc: CompilerContext) extends TypeTransformer {

  override def transform(kind: Type): Type = kind match {
    case TypeRef(Sym(symbol, Nil)) => walk(symbol.kind)

    case TypeRef(ref: Ref) => cc.ice(ref, "Cannot de-reference TypeRef")

    case _ => kind
  }
}
