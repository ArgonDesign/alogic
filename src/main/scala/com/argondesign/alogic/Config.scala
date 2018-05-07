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
// Static compiler config
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

object Config {

  final val allowWidthInference = false

  final val checkAssignWidth = false

  final val binaryOpWidthWarnings = false

  final val treatNumAs32Wide = true

  final val applyTransformChecks = true
}
