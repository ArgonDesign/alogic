////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Run-time compiler settings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.core.enums.UninitializedLocals

import java.nio.file.Path

case class Settings(
    // Directories to search for imported packages
    importSearchDirs: List[Path] = Nil,
    // Output directory
    oPath: Option[Path] = None,
    // The field separator sequence
    sep: String = "__",
    // The strategy for handling uninitialized local variables
    uninitialized: UninitializedLocals.Type = UninitializedLocals.None,
    // Output prefix to use
    ensurePrefix: String = "",
    // Maximum permitted output module name
    outputNameMaxLength: Option[Int] = None,
    // Header text to prepend to output files
    header: String = "",
    // Colorize diagnostic messages
    colorize: Boolean = false,
    // Dump trees after each pass
    dumpTrees: Boolean = false,
    // Measure and report inserted execution timing
    profile: Boolean = false,
    // Reset style
    resetStyle: ResetStyle.Type = ResetStyle.AsyncLow,
    // Reset all
    resetAll: Boolean = true,
    // Gen loop iteration limit
    genLoopLimit: Int = 1024,
    // Combinational function recursion limit
    combRecLimit: Int = 16,
    // Enable LowerAssertions
    assertions: Boolean = true,
    // Emit stats
    stats: Boolean = false,
    // Sandbox path for file accesses, if any
    sandboxPathOpt: Option[Path] = None,
    //
    parallel: Boolean = false,
    // Emit schematic diagrams
    schematic: Boolean = false) {
  require(sandboxPathOpt.forall(path => path.toFile.getCanonicalFile.toPath == path))
}
