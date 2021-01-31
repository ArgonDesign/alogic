////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Compile single .alogic files under src/test/resources/compile/single as tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

abstract class CompileSingleBase(parallel: Boolean) extends CompilationTest(parallel) {
  private val basePath: Path = Paths.get(getClass.getResource("/compile/single").getPath)

  // For each source file under the test directory recursively
  Files
    .walk(basePath)
    .filter(_.toString.endsWith(".alogic"))
    .sorted
    .forEach(path => defineTest(basePath.relativize(path).toString, path.toFile))
}
