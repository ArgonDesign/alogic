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
import com.argondesign.alogic.core.Symbols.Symbol

trait DeclSingletonOps { this: DeclSingleton =>

  final lazy val ports: List[Symbol] = decls collect {
    case decl: DeclIn      => decl.symbol
    case decl: DeclOut     => decl.symbol
    case decl: DeclPipeIn  => decl.symbol
    case decl: DeclPipeOut => decl.symbol
  }

}
