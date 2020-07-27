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
// Compile single alogic files as tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.nio.file.Paths

final class CompileSingle extends CompilationTest {

  private val base = "/compile/single"

  private val testPath = Paths.get(getClass.getResource(base).getPath)

  // For each source file in the test directory
  (testPath.toFile.listFiles filter { _.getPath.endsWith(".alogic") }).sorted foreach { file =>
    defineTest(file.getAbsolutePath)
  }
}
