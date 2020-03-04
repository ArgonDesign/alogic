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

import java.io.File

final class CompileSingle extends CompilationTest {

  val base = "/compile/single"

  val testPath = new File(getClass.getResource(base).getPath)

  // For each source file in the test directory
  testPath.listFiles.sorted foreach { file =>
    // Figure out the name of the entity (also filters out non matching files)
    """.*/(.*)\.alogic$""".r findFirstMatchIn file.getPath foreach { matchData =>
      // Name of top level
      val top = matchData.group(1)

      // Name of check file (same as source file)
      val checkFile = matchData.group(0)

      // Define test
      defineTest(s"${base.tail}/${top}.alogic", testPath, top, checkFile)
    }
  }
}
