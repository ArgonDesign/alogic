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
// Builtin '@randbit' that returns a random bit. Useful for testing.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

object AtRandbit extends BuiltinPolyFunc {

  protected def name = "@randbit"

  protected def retType(args: List[Expr])(implicit cc: CompilerContext): Type = TypeUInt(Expr(1))

  protected def validArgs(args: List[Expr])(implicit cc: CompilerContext) = args.isEmpty

}
