////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Input reading
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

trait Input { this: CompilerContext =>

  // Read the text file and return Right(content as string),
  // or Left(error message)
  def readFile(file: File): Either[String, String] = {
    val canonicalPath = file.getCanonicalFile.toPath
    if (settings.sandboxPathOpt.exists(sandboxPath => !canonicalPath.startsWith(sandboxPath))) {
      Left("Imported file is outside sandbox")
    } else {
      Right(new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8))
    }
  }

}
