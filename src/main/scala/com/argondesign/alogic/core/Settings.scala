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

import java.io.File
import java.io.PrintWriter

import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.core.enums.UninitializedLocals

case class Settings(
    // Directories to search for alogic files
    moduleSearchDirs: List[File] = Nil,
    // Directories to search for preprocessor include files
    includeSearchDirs: List[File] = Nil,
    // Initial preprocessor definitions
    initialDefines: Map[String, String] = Map.empty,
    // Output writer factory
    outputWriterFactory: Either[(Tree, String), String] => PrintWriter = { _ =>
      ???
    },
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
    // For debugging only, trace the progress of elaboration
    traceElaborate: Boolean = false)
