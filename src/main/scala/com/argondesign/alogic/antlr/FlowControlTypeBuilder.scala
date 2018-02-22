////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a FlowControlType from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.Flow_control_typeContext
import com.argondesign.alogic.antlr.AlogicParser.FlowControlTypeSyncAcceptContext
import com.argondesign.alogic.antlr.AlogicParser.FlowControlTypeSyncContext
import com.argondesign.alogic.antlr.AlogicParser.FlowControlTypeSyncReadyContext
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlType
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid

object FlowControlTypeBuilder extends BaseBuilder[Flow_control_typeContext, FlowControlType] {

  def apply(ctx: Flow_control_typeContext)(implicit cc: CompilerContext): FlowControlType = {
    object Visitor extends AlogicScalarVisitor[FlowControlType] {
      override def defaultResult = FlowControlTypeNone
      override def visitFlowControlTypeSync(ctx: FlowControlTypeSyncContext) = FlowControlTypeValid
      override def visitFlowControlTypeSyncReady(ctx: FlowControlTypeSyncReadyContext) = FlowControlTypeReady
      override def visitFlowControlTypeSyncAccept(ctx: FlowControlTypeSyncAcceptContext) = FlowControlTypeAccept
    }

    Visitor(ctx)
  }

}
