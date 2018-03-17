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
// Builtin '@sx'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

object AtSx extends BuiltinPolyFunc {

  protected def name = "@sx"

  protected def retType(args: List[Expr], cc: CompilerContext): Type = {
    TypeInt(args(1).tpe.isSigned, args(0))
  }

  protected def validArgs(args: List[Expr], cc: CompilerContext) = {
    args.lengthCompare(2) == 0 && args(0).isKnownConst(cc) && args(1).tpe.isPacked
  }

}
