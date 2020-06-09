////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a Desc AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.enums.EntityVariant

import scala.util.ChainingSyntax
import scala.jdk.CollectionConverters._

object DescBuilder extends BaseBuilder[DescContext, Desc] with ChainingSyntax {

  def apply(ctx: DescContext)(implicit cc: CompilerContext, sc: SourceContext): Desc = {
    object FCTVisitor extends AlogicScalarVisitor[FlowControlType] {
      override def defaultResult: FlowControlType =
        FlowControlTypeNone
      override def visitFCTSync(ctx: FCTSyncContext): FlowControlType =
        FlowControlTypeValid
      override def visitFCTSyncReady(ctx: FCTSyncReadyContext): FlowControlType =
        FlowControlTypeReady
      override def visitFCTSyncAccept(ctx: FCTSyncAcceptContext): FlowControlType =
        FlowControlTypeAccept
    }

    object STTVisitor extends AlogicScalarVisitor[StorageType] {
      override def defaultResult: StorageType =
        StorageTypeDefault
      override def visitSTTWire(ctx: STTWireContext): StorageType =
        StorageTypeWire
      override def visitSTTSlices(ctx: STTSlicesContext): StorageType =
        StorageTypeSlices(SlicesBuilder(ctx.slices))
    }

    def argDescs(ctx: Formal_argumentsContext): List[DescVar] =
      if (ctx == null) {
        Nil
      } else {
        List from {
          ctx.expr.iterator.asScala zip ctx.IDENTIFIER.iterator.asScala map {
            case (e, i) =>
              val loc = i.loc.copy(start = e.loc.start)
              DescVar(IdentBuilder(i), ExprBuilder(e), None) withLoc loc
          }
        }
      }

    object Visitor extends AlogicScalarVisitor[Desc] {
      //////////////////////////////////////////////////////////////////////////
      // Attach attributes
      //////////////////////////////////////////////////////////////////////////

      override def visitDesc(ctx: DescContext): Desc =
        if (ctx.attributes != null) {
          visit(ctx.descbase) tap {
            _.ref.asInstanceOf[Ident].attr addAll AttrBuilder(ctx.attributes.attr)
          }
        } else {
          visit(ctx.descbase)
        }

      //////////////////////////////////////////////////////////////////////////
      // Desc
      //////////////////////////////////////////////////////////////////////////

      override def visitDescVar(ctx: DescVarContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr(0))
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescVar(ident, spec, initOpt) withLoc loc
      }

      override def visitDescIn(ctx: DescInContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val fct = FCTVisitor(ctx.fct)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescIn(ident, spec, fct) withLoc loc
      }

      override def visitDescOut(ctx: DescOutContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr(0))
        val fct = FCTVisitor(ctx.fct)
        val stt = STTVisitor(ctx.stt)
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescOut(ident, spec, fct, stt, initOpt) withLoc loc
      }

      override def visitDescPipeline(ctx: DescPipelineContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescPipeline(ident, spec) withLoc loc
      }

      override def visitDescParam(ctx: DescParamContext): Desc = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val spec = ExprBuilder(ctx.expr(0))
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescParam(ident, spec, initOpt) withLoc loc
      }

      override def visitDescParamType(ctx: DescParamTypeContext): Desc = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescParamType(ident, initOpt) withLoc loc
      }

      override def visitDescConst(ctx: DescConstContext): Desc = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val spec = ExprBuilder(ctx.expr(0))
        val init = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescConst(ident, spec, init) withLoc loc
      }

      override def visitDescArr(ctx: DescArrContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val elem = ExprBuilder(ctx.expr(0))
        val size = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescArray(ident, elem, size) withLoc loc
      }

      override def visitDescSram(ctx: DescSramContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val elem = ExprBuilder(ctx.expr(0))
        val size = ExprBuilder(ctx.expr(1))
        val stt = if (ctx.wire != null) StorageTypeWire else StorageTypeReg
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescSram(ident, elem, size, stt) withLoc loc
      }

      override def visitDescType(ctx: DescTypeContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescType(ident, spec) withLoc loc
      }

      override def visitDescEntity(ctx: DescEntityContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val variant = ctx.entity_keyword.text match {
          case "fsm"     => EntityVariant.Fsm
          case "network" => EntityVariant.Net
          case _         => EntityVariant.Ver
        }
        val body = EntBuilder(ctx.ent)(cc, SourceContext.Entity)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescEntity(ident, variant, body) withLoc loc
      }

      override def visitDescRecord(ctx: DescRecordContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val body = RecBuilder(ctx.rec)(cc, SourceContext.Record)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescRecord(ident, body) withLoc loc
      }

      override def visitDescInstance(ctx: DescInstanceContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescInstance(ident, spec) withLoc loc
      }

      override def visitDescSingleton(ctx: DescSingletonContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val variant = ctx.entity_keyword.text match {
          case "fsm"     => EntityVariant.Fsm
          case "network" => EntityVariant.Net
          case _         => EntityVariant.Ver
        }
        val body = EntBuilder(ctx.ent)(cc, SourceContext.Entity)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescSingleton(ident, variant, body) withLoc loc
      }

      override def visitDescFuncAlogic(ctx: DescFuncAlogicContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val variant = if (ctx.stat == null) FuncVariant.None else FuncVariant.Static
        val ret = ExprBuilder(ctx.expr)
        val args = argDescs(ctx.formal_arguments)
        val body = StmtBuilder(ctx.stmt)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescFunc(ident, variant, ret, args, body) withLoc loc
      }

      override def visitDescFuncImport(ctx: DescFuncImportContext): Desc = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val ret = ExprBuilder(ctx.expr)
        val args = argDescs(ctx.formal_arguments)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescFunc(ident, FuncVariant.Xeno, ret, args, Nil) withLoc loc
      }
    }

    Visitor(ctx)
  }

}
