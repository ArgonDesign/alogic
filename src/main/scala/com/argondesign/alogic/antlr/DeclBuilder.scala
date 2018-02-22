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
import com.argondesign.alogic.antlr.AlogicParser.DeclVarContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclConstContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclInContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclOutContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclParamContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclPipelineContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclTermContext
import com.argondesign.alogic.antlr.AlogicParser.EntityDeclVerilogContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.DeclIdent
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageSliceFReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeArray
import com.argondesign.alogic.core.Types.TypeConst
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.Types.TypeParam
import com.argondesign.alogic.core.Types.TypePipeline
import com.argondesign.alogic.core.Types.TypeVerilog
import com.argondesign.alogic.util.unreachable

import org.antlr.v4.runtime.ParserRuleContext

object DeclBuilder extends BaseBuilder[ParserRuleContext, DeclIdent] {

  def apply(ctx: ParserRuleContext)(implicit cc: CompilerContext): DeclIdent = {
    object Visitor extends AlogicScalarVisitor[DeclIdent] {
      // Simple decls
      override def visitDeclVar(ctx: DeclVarContext) = {
        val kind = TypeBuilder(ctx.kind)
        val init = Option(ctx.expr) map { ExprBuilder(_) }
        DeclIdent(ctx.IDENTIFIER, kind, init)
      }

      override def visitDeclArr(ctx: DeclArrContext) = {
        val sizes = ExprBuilder(ctx.expr).reverse
        val kind = sizes.foldLeft[Type](TypeBuilder(ctx.kind)) { (elem, size) => TypeArray(elem, size) }
        DeclIdent(ctx.IDENTIFIER, kind, None)
      }

      // Entity decls
      override def visitEntityDeclOut(ctx: EntityDeclOutContext) = {
        val name = ctx.IDENTIFIER.text
        val underlying = TypeBuilder(ctx.kind)
        val fcType = FlowControlTypeBuilder(ctx.flow_control_type)
        val storageOpt = Option(ctx.storage_type) map { StorageTypeBuilder(_) }
        val storage = (fcType, storageOpt) match {
          // Defaults
          case (FlowControlTypeNone, None)  => StorageTypeReg
          case (FlowControlTypeValid, None) => StorageTypeReg
          case (_, None)                    => StorageTypeSlices(List(StorageSliceFReg))
          // Error checks
          case (FlowControlTypeNone, Some(_: StorageTypeSlices)) => {
            cc.error(ctx, s"Output port '${name}' without flow control cannot use output slices")
            StorageTypeReg
          }
          case (FlowControlTypeValid, Some(_: StorageTypeSlices)) => {
            cc.error(ctx, s"Output port '${name}' with 'sync' flow control cannot use output slices")
            StorageTypeReg
          }
          // Unbox
          case (_, Some(storageType)) => storageType
        }
        val kind = TypeOut(underlying, fcType, storage)
        DeclIdent(name, kind, None)
      }

      override def visitEntityDeclIn(ctx: EntityDeclInContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val fcType = FlowControlTypeBuilder(ctx.flow_control_type)
        val kind = TypeIn(underlying, fcType)
        DeclIdent(ctx.IDENTIFIER, kind, None)
      }

      override def visitEntityDeclParam(ctx: EntityDeclParamContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypeParam(underlying)
        val init = ExprBuilder(ctx.expr)
        DeclIdent(ctx.IDENTIFIER, kind, Some(init))
      }

      override def visitEntityDeclConst(ctx: EntityDeclConstContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypeConst(underlying)
        val init = ExprBuilder(ctx.expr)
        DeclIdent(ctx.IDENTIFIER, kind, Some(init))
      }

      override def visitEntityDeclPipeline(ctx: EntityDeclPipelineContext) = {
        val underlying = TypeBuilder(ctx.kind)
        val kind = TypePipeline(underlying)
        DeclIdent(ctx.IDENTIFIER, kind, None)
      }

      override def visitEntityDeclTerm(ctx: EntityDeclTermContext) = visit(ctx.decl)

      override def visitEntityDeclVerilog(ctx: EntityDeclVerilogContext) = {
        val decl = visit(ctx.decl) match {
          case decl: DeclIdent => decl
          case _               => unreachable
        }
        if (decl.init.isDefined) {
          cc.error(ctx, s"Verilog variable '${decl.name}' cannot have initializer")
        }
        DeclIdent(decl.name, TypeVerilog(decl.kind), None)
      }
    }

    Visitor(ctx)
  }

}
