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
import java.io.Writer
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.core.enums.UninitializedLocals

case class Settings(
    // Directories to search for alogic files
    moduleSearchDirs: List[File] = Nil,
    // Directories to search for preprocessor include files
    includeSearchDirs: List[File] = Nil,
    // Initial preprocessor definitions
    initialDefines: Map[String, String] = Map.empty,
    // Entity writer factory
    entityWriterFactory: (Entity, String) => Writer = {
      case (_: Entity, _: String) => ???
    },
    // The field separator sequence
    sep: String = "_",
    // The strategy for handling uninitialized local variables
    uninitialized: UninitializedLocals.Type = UninitializedLocals.None,
    // Output prefix to use
    ensurePrefix: String = "",
    // Header text to prepend to output files
    header: String = "",
    // Colourize diagnostic messages
    colourize: Boolean = false,
    // Dump trees after each pass
    dumpTrees: Boolean = true,
    // Module manifest output path
    moduleManifestPath: Option[Path] = None,
    // Reset style
    resetStyle: ResetStyle.Type = ResetStyle.AsyncLow
)
