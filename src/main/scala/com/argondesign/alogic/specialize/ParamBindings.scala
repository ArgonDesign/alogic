////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
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

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.Trees.Expr

sealed trait ParamBindings

case class ParamBindingsNamed(params: Map[(String, List[BigInt]), Expr]) extends ParamBindings
case class ParamBindingsPositional(params: List[Expr]) extends ParamBindings
