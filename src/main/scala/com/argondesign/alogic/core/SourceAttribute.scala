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
// Representations of various source level attributes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees
import com.argondesign.alogic.core.StorageTypes.StorageSlice

sealed trait SourceAttribute extends Locationed

object SourceAttribute {
  final case class Flag() extends SourceAttribute
  final case class Expr(expr: Trees.Expr) extends SourceAttribute
  final case class Slices(slices: List[StorageSlice]) extends SourceAttribute
}
