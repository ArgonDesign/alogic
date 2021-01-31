////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Top level definitions for passes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Defn
import com.argondesign.alogic.core.ParOrSeqIterable

package object passes {
  type Pairs = ParOrSeqIterable[(Decl, Defn)]
}
