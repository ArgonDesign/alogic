////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Common members of Types.Type
// These are factored out into a separate file to keep Type.Types readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import Types._

trait TypeOps { this: Type =>
  ////////////////////////////////////////////////////////////////////////////////
  // Rewrie with TypeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  def rewrite(tt: TypeTransformer): Type = tt(this)
}
