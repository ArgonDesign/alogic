////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

sealed trait SourceContext

object SourceContext {
  case object Package extends SourceContext
  case object Entity extends SourceContext
  case object Record extends SourceContext
  case object FuncCtrl extends SourceContext
  case object FuncComb extends SourceContext
  case object Unknown extends SourceContext
}
