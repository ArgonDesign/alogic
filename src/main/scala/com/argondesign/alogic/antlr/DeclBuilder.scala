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
// Build a Decl AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.DeclRef
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Types._

object DeclBuilder extends BaseBuilder[DeclContext, DeclRef] {

  def apply(ctx: DeclContext)(implicit cc: CompilerContext): DeclRef = {
    object Visitor extends AlogicScalarVisitor[DeclRef] {

      // Attach initializers
      override def visitDecl(ctx: DeclContext) = {
        val init = Option(ctx.expr) map { ExprBuilder(_) }
        val decl @ DeclRef(ref, _, _) = visit(ctx.declbase)
        if (ctx.attr != null) {
          ref.asInstanceOf[Ident] withAttr AttrBuilder(ctx.attr)
        }
        if (init.isDefined) {
          decl.copy(init = init) withLoc ctx.loc.copy(point = decl.loc.point)
        } else {
          decl
        }
      }

      // Simple decls
      override def visitDeclVar(ctx: DeclVarContext) = {
        val kind = TypeBuilder(ctx.kind)
        DeclRef(IdentBuilder(ctx.ident), kind, None) withLoc ctx.loc
      }

      // Entity decls
      override def visitDeclOut(ctx: DeclOutContext) = {
        val ident = IdentBuilder(ctx.ident)
        val underlying = TypeBuilder(ctx.kind)
        val fcType = FlowControlTypeBuilder(ctx.flow_control_type)
        val storage = StorageTypeBuilder(ctx.storage_type)
        val kind = TypeOut(underlying, fcType, storage)
        DeclRef(ident, kind, None) withLoc {
          ctx.loc.copy(point = ctx.ident.start.getStartIndex)
        }
      }

      override def visitDeclIn(ctx: DeclInContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val fcType = FlowControlTypeBuilder(ctx.flow_control_type)
        val kind = TypeIn(underlying, fcType)
        DeclRef(IdentBuilder(ctx.ident), kind, None) withLoc {
          ctx.loc.copy(point = ctx.ident.start.getStartIndex)
        }
      }

      override def visitDeclParam(ctx: DeclParamContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypeParam(underlying)
        DeclRef(IdentBuilder(ctx.IDENTIFIER), kind, None) withLoc {
          ctx.loc.copy(point = ctx.IDENTIFIER.getStartIndex)
        }
      }

      override def visitDeclConst(ctx: DeclConstContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypeConst(underlying)
        DeclRef(IdentBuilder(ctx.IDENTIFIER), kind, None) withLoc {
          ctx.loc.copy(point = ctx.IDENTIFIER.getStartIndex)
        }
      }

      override def visitDeclPipeline(ctx: DeclPipelineContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypePipeline(underlying)
        DeclRef(IdentBuilder(ctx.ident), kind, None) withLoc {
          ctx.loc.copy(point = ctx.ident.start.getStartIndex)
        }
      }

      override def visitDeclArr(ctx: DeclArrContext) = {
        val elem = TypeBuilder(ctx.kind)
        val size = ExprBuilder(ctx.expr)
        val kind = TypeArray(elem, size)
        DeclRef(IdentBuilder(ctx.ident), kind, None) withLoc {
          ctx.loc.copy(point = ctx.ident.start.getStartIndex)
        }
      }

      override def visitDeclSram(ctx: DeclSramContext) = {
        val elem = TypeBuilder(ctx.kind)
        val size = ExprBuilder(ctx.expr)
        val st = if (ctx.wire != null) StorageTypeWire else StorageTypeReg
        val kind = TypeSram(elem, size, st)
        DeclRef(IdentBuilder(ctx.ident), kind, None) withLoc {
          ctx.loc.copy(point = ctx.ident.start.getStartIndex)
        }
      }
    }

    Visitor(ctx)
  }

}
