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

import java.io.File

final class CompileMultiple extends CompilationTest {

  val base = "/compile/multi"

  val testPath = new File(getClass.getResource(base).getPath)

  // Fore each subdirectory in the test directory
  (testPath.listFiles filter { _.isDirectory }).sorted foreach { dir =>
    // Name of check file (same as top level file)
    val checkFile = dir.toPath.resolve("top.alogic").toString

    // Define test
    defineTest(s"${base.tail}/${dir.getName}", dir, "top", checkFile)
  }
}
