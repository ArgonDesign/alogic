////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build a Desc AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.util.unreachable

import scala.jdk.CollectionConverters._
import scala.util.ChainingSyntax

object DescBuilder extends BaseBuilder[DescContext, Desc] with ChainingSyntax {

  def apply(ctx: DescContext)(implicit mb: MessageBuffer, sc: SourceContext): Desc = {
    object FCTVisitor extends AlogicScalarVisitor[FlowControlType] {
      override def defaultResult: FlowControlType =
        FlowControlTypeNone
      override def visitFCTSync(ctx: FCTSyncContext): FlowControlType =
        FlowControlTypeValid
      override def visitFCTSyncReady(ctx: FCTSyncReadyContext): FlowControlType =
        FlowControlTypeReady
    }

    object STTVisitor extends AlogicScalarVisitor[StorageType] {
      override def defaultResult: StorageType =
        StorageTypeDefault
      override def visitSTTWire(ctx: STTWireContext): StorageType =
        StorageTypeWire
      override def visitSTTSlices(ctx: STTSlicesContext): StorageType =
        StorageTypeSlices(SlicesBuilder(ctx.slices))
    }

    object GenItemVisitor extends AlogicScalarVisitor[Tree] {
      // format: off
      override def visitGenItemDesc(ctx: GenItemDescContext): Tree = DescBuilder(ctx.desc)
      override def visitGenItemImport(ctx: GenItemImportContext): Tree = ImportBuilder(ctx.imprt)
      override def visitGenItemUsing(ctx: GenItemUsingContext): Tree = UsingBuilder(ctx.usng)
      override def visitGenItemFrom(ctx: GenItemFromContext): Tree = FromBuilder(ctx.from)
      override def visitGenItemAssertion(ctx: GenItemAssertionContext): Tree = AssertionBuilder(ctx.assertion)
      override def visitGenItemPkg(ctx: GenItemPkgContext): Tree = PkgBuilder(ctx.pkg)
      override def visitGenItemEnt(ctx: GenItemEntContext): Tree = EntBuilder(ctx.ent)
      // Currently there are no Rec items that are not covered by Desc/Import/Using/From/Assertion
      override def visitGenItemRec(ctx: GenItemRecContext): Tree = unreachable
      override def visitGenItemStmt(ctx: GenItemStmtContext): Tree = StmtBuilder(ctx.stmt)
      override def visitGenItemCase(ctx: GenItemCaseContext): Tree = CaseBuilder(ctx.kase)
      // format: on
    }

    object GinitVisitor extends AlogicScalarVisitor[DescGenVar] {
      override def visitGinit(ctx: GinitContext): DescGenVar = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val spec = ExprBuilder(ctx.expr(0))
        val init = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescGenVar(ident, Nil, spec, init) withLoc loc
      }
    }

    def argDescs(ctx: Formal_argumentsContext): List[DescVar] =
      if (ctx == null) {
        Nil
      } else {
        List from {
          ctx.expr.iterator.asScala zip ctx.IDENTIFIER.iterator.asScala map {
            case (e, i) =>
              val loc = i.loc.copy(start = e.loc.start)
              DescVar(IdentBuilder(i), Nil, ExprBuilder(e), None) withLoc loc
          }
        }
      }

    def identOrEmpty(ctx: IdentContext): Ident =
      if (ctx != null) IdentBuilder(ctx) else Ident("", Nil) withLoc Loc.synthetic

    object Visitor extends AlogicScalarVisitor[Desc] {
      //////////////////////////////////////////////////////////////////////////
      // Attach attributes
      //////////////////////////////////////////////////////////////////////////

      override def visitDesc(ctx: DescContext): Desc = {
        val desc = visit(ctx.descbase)
        if (ctx.attributes == null) {
          desc
        } else {
          val attr = AttrBuilder(ctx.attributes.attr)
          desc.copyAttr(attr) withLocOf desc
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Desc
      //////////////////////////////////////////////////////////////////////////

      private def identOrElse(ctx: IdentContext, ident: => Ident): Ident =
        if (ctx != null) IdentBuilder(ctx) else ident

      override def visitDescVar(ctx: DescVarContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr(0))
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        if (ctx.sttc != null) {
          DescStatic(ident, Nil, spec, initOpt) withLoc loc
        } else {
          DescVar(ident, Nil, spec, initOpt) withLoc loc
        }
      }

      override def visitDescIn(ctx: DescInContext): Desc = {
        val ident = identOrElse(ctx.ident, Ident("in", Nil) withLoc ctx.in.loc)
        val fct = FCTVisitor(ctx.fct)
        val loc = ctx.loc.copy(point = ident.loc.start)
        if (ctx.spec != null) {
          val spec = ExprBuilder(ctx.spec)
          DescIn(ident, Nil, spec, fct) withLoc loc
        } else {
          DescPipeIn(ident, Nil, fct) withLoc loc
        }
      }

      override def visitDescOut(ctx: DescOutContext): Desc = {
        val ident = identOrElse(ctx.ident, Ident("out", Nil) withLoc ctx.out.loc)
        val fct = FCTVisitor(ctx.fct)
        val stt = STTVisitor(ctx.stt)
        val loc = ctx.loc.copy(point = ident.loc.start)
        if (ctx.spec != null) {
          val spec = ExprBuilder(ctx.spec)
          val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
          DescOut(ident, Nil, spec, fct, stt, initOpt) withLoc loc
        } else {
          if (ctx.init != null) {
            mb.error(ctx.init, "Pipeline output port cannot have an initializer")
          }
          DescPipeOut(ident, Nil, fct, stt) withLoc loc
        }
      }

      override def visitDescPipeVar(ctx: DescPipeVarContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescPipeVar(ident, Nil, spec) withLoc loc
      }

      override def visitDescParam(ctx: DescParamContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr(0))
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescParam(ident, Nil, spec, initOpt, finished = false) withLoc loc
      }

      override def visitDescParamType(ctx: DescParamTypeContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val initOpt = if (ctx.init != null) Some(ExprBuilder(ctx.init)) else None
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescParamType(ident, Nil, initOpt, finished = false) withLoc loc
      }

      override def visitDescConst(ctx: DescConstContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr(0))
        val init = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        sc match {
          case SourceContext.Package | SourceContext.Entity | SourceContext.Record =>
            DescConst(ident, Nil, spec, init) withLoc loc
          case SourceContext.FuncCtrl | SourceContext.FuncComb =>
            DescVal(ident, Nil, spec, init) withLoc loc
          case SourceContext.Unknown =>
            throw Ice("Cannot parse 'const' definition without source context")
        }
      }

      override def visitDescArr(ctx: DescArrContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val elem = ExprBuilder(ctx.expr(0))
        val size = ExprBuilder(ctx.expr(1))
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescArray(ident, Nil, elem, size) withLoc loc
      }

      override def visitDescSram(ctx: DescSramContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val elem = ExprBuilder(ctx.expr(0))
        val size = ExprBuilder(ctx.expr(1))
        val stt = if (ctx.wire != null) StorageTypeWire else StorageTypeReg
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescSram(ident, Nil, elem, size, stt) withLoc loc
      }

      override def visitDescType(ctx: DescTypeContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescType(ident, Nil, spec) withLoc loc
      }

      override def visitDescEntity(ctx: DescEntityContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val variant = ctx.entity_keyword.txt match {
          case "fsm"     => EntityVariant.Fsm
          case "network" => EntityVariant.Net
          case _         => EntityVariant.Ver
        }
        val body = EntBuilder(ctx.ent)(mb, SourceContext.Entity)
        val loc = ident.loc.copy(start = ctx.loc.start)
        DescEntity(ident, Nil, variant, body) withLoc loc
      }

      override def visitDescRecord(ctx: DescRecordContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val body = RecBuilder(ctx.rec)(mb, SourceContext.Record)
        val loc = ident.loc.copy(start = ctx.loc.start)
        DescRecord(ident, Nil, body) withLoc loc
      }

      override def visitDescInstance(ctx: DescInstanceContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val spec = ExprBuilder(ctx.expr)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescInstance(ident, Nil, spec) withLoc loc
      }

      override def visitDescSingleton(ctx: DescSingletonContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val variant = ctx.entity_keyword.txt match {
          case "fsm"     => EntityVariant.Fsm
          case "network" => EntityVariant.Net
          case _         => EntityVariant.Ver
        }
        val body = EntBuilder(ctx.ent)(mb, SourceContext.Entity)
        val loc = ident.loc.copy(start = ctx.loc.start)
        DescSingleton(ident, Nil, variant, body) withLoc loc
      }

      override def visitDescFuncAlogic(ctx: DescFuncAlogicContext): Desc = {
        val ident = IdentBuilder(ctx.ident)
        val variant = if (ctx.stat != null) {
          if (sc != SourceContext.Record) {
            mb.error(ctx.stat.loc, "No 'static' modifier is allowed here")
          }
          FuncVariant.Static
        } else {
          sc match {
            case SourceContext.Entity => FuncVariant.Ctrl
            case SourceContext.Record => FuncVariant.Method
            case SourceContext.Package | SourceContext.FuncCtrl | SourceContext.FuncComb =>
              FuncVariant.Comb
            case SourceContext.Unknown =>
              throw Ice("Cannot parse function definition without source context")
          }
        }
        val ret = ExprBuilder(ctx.expr)
        val args = argDescs(ctx.formal_arguments)
        val body = {
          val subSC = sc match {
            case SourceContext.Entity => SourceContext.FuncCtrl
            case SourceContext.Package | SourceContext.Record | SourceContext.FuncCtrl |
                SourceContext.FuncComb =>
              SourceContext.FuncComb
            case SourceContext.Unknown =>
              throw Ice("Cannot parse function definition without source context")
          }
          StmtBuilder(ctx.stmt)(mb, subSC)
        }
        val loc = ctx.loc.copy(point = ident.loc.start, end = ctx.cpar.loc.start)
        DescFunc(ident, Nil, variant, ret, args, body) withLoc loc
      }

      override def visitDescFuncImport(ctx: DescFuncImportContext): Desc = {
        val ident = IdentBuilder(ctx.IDENTIFIER)
        val ret = ExprBuilder(ctx.expr)
        val args = argDescs(ctx.formal_arguments)
        val loc = ctx.loc.copy(point = ident.loc.start)
        DescFunc(ident, Nil, FuncVariant.Xeno, ret, args, Nil) withLoc loc
      }

      override def visitDescGenIf(ctx: DescGenIfContext): Desc = {
        val ident = identOrEmpty(ctx.ident)
        val cs = ctx.conds.iterator.asScala map { c => ExprBuilder(c) }
        val tss = ctx.thenItemss.iterator.asScala map { g => GenItemVisitor(g.genitem) }
        val cases = cs zip tss map {
          case (c, ts) =>
            val end = ts.lastOption map { _.loc.end } getOrElse c.loc.end
            GenCase(c, ts) withLoc c.loc.copy(end = end)
        }
        val defaults = if (ctx.elseItems != null) GenItemVisitor(ctx.elseItems.genitem) else Nil
        val loc = ctx.loc.copy(end = (if (ctx.ident == null) ctx.cpar.loc else ident.loc).end)
        DescGenIf(ident, Nil, List.from(cases), defaults) withLoc loc
      }

      override def visitDescGenFor(ctx: DescGenForContext): Desc = {
        val ident = identOrEmpty(ctx.ident)
        val inits = GinitVisitor(ctx.ginits.ginit)
        val cond = ExprBuilder(ctx.expr)
        val steps = StmtBuilder(ctx.lsteps.lstep)
        val body = GenItemVisitor(ctx.genitems.genitem)
        val loc = ctx.loc.copy(end = (if (ctx.ident == null) ctx.cpar.loc else ident.loc).end)
        DescGenFor(ident, Nil, inits, cond, steps, body) withLoc loc
      }

      override def visitDescGenRange(ctx: DescGenRangeContext): Desc = {
        val ident = identOrEmpty(ctx.ident)
        val init = {
          val ident = IdentBuilder(ctx.IDENTIFIER)
          val spec = ExprBuilder(ctx.expr(0))
          val init = ExprNum(false, 0) withLoc ident.loc
          val loc = ctx.loc.copy(point = ident.loc.start)
          DescGenVar(ident, Nil, spec, init) withLoc loc
        }
        val end = ExprBuilder(ctx.expr(1))
        val body = GenItemVisitor(ctx.genitems.genitem)
        val loc = ctx.loc.copy(end = (if (ctx.ident == null) ctx.cpar.loc else ident.loc).end)
        DescGenRange(ident, Nil, init, ctx.op.txt, end, body) withLoc loc
      }

    }

    Visitor(ctx)
  }

}
