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
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

trait DefnFuncOps { this: DefnFunc =>

  override final lazy val descs: List[Desc] = body collect { case StmtDesc(desc) => desc }

  final lazy val decls: List[Decl] = body collect { case StmtDecl(decl) => decl }

  final lazy val defns: List[Defn] = body collect { case StmtDefn(defn) => defn }

}
