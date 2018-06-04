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
// Run-time compiler settings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.nio.file.Path

case class Settings(
    // Output directory
    odir: Option[Path] = None,
    // Source base path
    srcbase: Option[Path] = None,
    // The field separator sequence
    sep: String = "_",
    // The strategy for handling uninitialized local variables
    uninitialized: String = "none",
    // Output prefix to use
    ensurePrefix: String = "",
    // Colourize diagnostic messages
    colourize: Boolean = false,
    // Dump trees after each pass
    dumpTrees: Boolean = true
)
