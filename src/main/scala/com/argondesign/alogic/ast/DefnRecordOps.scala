////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

trait DefnRecordOps { this: DefnRecord =>

  final lazy val decls: List[Decl] = body collect { case RecSplice(decl: Decl) => decl }

  final override lazy val defns: List[Defn] = body collect { case RecSplice(defn: Defn) => defn }

}
