////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Representations of flow control types
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

object FlowControlTypes {

  sealed abstract trait FlowControlType

  case object FlowControlTypeNone extends FlowControlType
  case object FlowControlTypeValid extends FlowControlType
  case object FlowControlTypeReady extends FlowControlType

}
