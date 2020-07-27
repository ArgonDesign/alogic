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
// Compile multiple alogic files as tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.nio.file.Paths

final class CompileMultiple extends CompilationTest {

  private val base = "/compile/multi"

  private val testPath = Paths.get(getClass.getResource(base).getPath)

  // Fore each subdirectory in the test directory
  (testPath.toFile.listFiles filter { _.isDirectory }).sorted foreach { dir =>
    defineTest(dir.toPath.resolve("top.alogic").toFile.getAbsolutePath)
  }
}
