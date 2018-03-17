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

import com.argondesign.alogic.antlr.AlogicParser.DeclArrContext
import com.argondesign.alogic.antlr.AlogicParser.DeclConstContext
import com.argondesign.alogic.antlr.AlogicParser.DeclContext
import com.argondesign.alogic.antlr.AlogicParser.DeclInContext
import com.argondesign.alogic.antlr.AlogicParser.DeclOutContext
import com.argondesign.alogic.antlr.AlogicParser.DeclParamContext
import com.argondesign.alogic.antlr.AlogicParser.DeclPipelineContext
import com.argondesign.alogic.antlr.AlogicParser.DeclVarContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageSliceFwd
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeArray
import com.argondesign.alogic.core.Types.TypeConst
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.Types.TypeParam
import com.argondesign.alogic.core.Types.TypePipeline

object DeclBuilder extends BaseBuilder[DeclContext, Decl] {

  def apply(ctx: DeclContext)(implicit cc: CompilerContext): Decl = {
    object Visitor extends AlogicScalarVisitor[Decl] {

      // Attach initializers
      override def visitDecl(ctx: DeclContext) = {
        val init = Option(ctx.expr) map { ExprBuilder(_) }
        val decl = visit(ctx.declbase)
        if (init.isDefined) {
          decl.copy(init = init) withLoc decl.loc
        } else {
          decl
        }
      }

      // Simple decls
      override def visitDeclVar(ctx: DeclVarContext) = {
        val kind = TypeBuilder(ctx.kind)
        Decl(ctx.IDENTIFIER.toIdent, kind, None) withLoc ctx.loc
      }

      override def visitDeclArr(ctx: DeclArrContext) = {
        val sizes = ExprBuilder(ctx.expr).reverse
        val kind = sizes.foldLeft[Type](TypeBuilder(ctx.kind)) { (elem, size) =>
          TypeArray(elem, size)
        }
        Decl(ctx.IDENTIFIER.toIdent, kind, None) withLoc ctx.loc
      }

      // Entity decls
      override def visitDeclOut(ctx: DeclOutContext) = {
        val ident = ctx.IDENTIFIER.toIdent
        val underlying = TypeBuilder(ctx.kind)
        val fcType = FlowControlTypeBuilder(ctx.flow_control_type)
        val storageOpt = Option(ctx.storage_type) map { StorageTypeBuilder(_) }
        val storage = (fcType, storageOpt) match {
          // Unbox
          case (_, Some(storageType)) => storageType
          // Defaults
          case (FlowControlTypeNone, None)  => StorageTypeReg
          case (FlowControlTypeValid, None) => StorageTypeReg
          case (_, None)                    => StorageTypeSlices(List(StorageSliceFwd))
        }

        val kind = TypeOut(underlying, fcType, storage)
        Decl(ident, kind, None) withLoc ctx.loc
      }

      override def visitDeclIn(ctx: DeclInContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val fcType = FlowControlTypeBuilder(ctx.flow_control_type)
        val kind = TypeIn(underlying, fcType)
        Decl(ctx.IDENTIFIER.toIdent, kind, None) withLoc ctx.loc
      }

      override def visitDeclParam(ctx: DeclParamContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypeParam(underlying)
        Decl(ctx.IDENTIFIER.toIdent, kind, None) withLoc ctx.loc
      }

      override def visitDeclConst(ctx: DeclConstContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypeConst(underlying)
        Decl(ctx.IDENTIFIER.toIdent, kind, None) withLoc ctx.loc
      }

      override def visitDeclPipeline(ctx: DeclPipelineContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypePipeline(underlying)
        Decl(ctx.IDENTIFIER.toIdent, kind, None) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
