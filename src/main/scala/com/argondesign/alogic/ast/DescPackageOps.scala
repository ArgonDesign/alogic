////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

import java.nio.file.Paths

trait DescPackageOps { this: DescPackage =>

  final lazy val descs: List[Desc] = body collect { case PkgSplice(desc: Desc) => desc }

  final def packageName: String = Paths.get(symbol.name).getFileName.toString.takeWhile(_ != '.')
}
