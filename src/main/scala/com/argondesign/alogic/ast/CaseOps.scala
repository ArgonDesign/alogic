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
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

trait CaseOps { this: Case =>

  def stmts: List[Stmt] = unreachable

}
