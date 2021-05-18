////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Visitor which generates list of SemanticTokens from the parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lsp

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AlogicParserRuleContext
import com.argondesign.alogic.antlr.AlogicScalarVisitor
import com.argondesign.alogic.antlr.AlogicToken
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.lsp.SemanticToken
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters._

class BaseTokenVisitor extends AlogicScalarVisitor[List[SemanticToken]] {
  override def defaultResult(): List[SemanticToken] = Nil

  def getTokens(tk: AlogicToken, typ: SemanticTokenType.Type): List[SemanticToken] =
    tk match {
      case null => Nil
      case _    => List(SemanticToken(tk.loc, typ))
    }

  def getTokens(ctx: AlogicParserRuleContext, typ: SemanticTokenType.Type): List[SemanticToken] =
    ctx match {
      case null => Nil
      case _    => List(SemanticToken(ctx.loc, typ))
    }

  def getTokens(nd: TerminalNode, typ: SemanticTokenType.Type): List[SemanticToken] =
    nd match {
      case null => Nil
      case _    => List(SemanticToken(nd.loc, typ))
    }

  override def aggregateResult(
      aggregate: List[SemanticToken],
      nextResult: List[SemanticToken]
    ): List[SemanticToken] = aggregate ++ nextResult

}

class SemanticTokenVisitor extends BaseTokenVisitor {
  private val parentVisitor = this

  val FunctionTokenVisitor = new BaseTokenVisitor {
    override def visitExprIndex(ctx: ExprIndexContext): List[SemanticToken] =
      visit(ctx.expr(0)) ++ parentVisitor(ctx.expr(1))

    override def visitExprAtid(ctx: ExprAtidContext): List[SemanticToken] =
      getTokens(ctx.ATID, SemanticTokenType.Function)

    override def visitExprDollarid(ctx: ExprDollaridContext): List[SemanticToken] =
      getTokens(ctx.DOLLARID, SemanticTokenType.Function)

    override def visitExprDot(ctx: ExprDotContext): List[SemanticToken] =
      parentVisitor(ctx.expr) ++ visit(ctx.ident)

    override def visitIdent(ctx: IdentContext): List[SemanticToken] =
      parentVisitor(ctx.expr).flatten ++
        getTokens(ctx.IDENTIFIER, SemanticTokenType.Function) ++
        getTokens(ctx.HASH, SemanticTokenType.Operator)

  }

  val TypeTokenVisitor = new BaseTokenVisitor {
    override def visitExprTypeBool(ctx: ExprTypeBoolContext): List[SemanticToken] =
      getTokens(ctx.BOOL, SemanticTokenType.Type)

    override def visitExprTypeSInt(ctx: ExprTypeSIntContext): List[SemanticToken] =
      getTokens(ctx.INTTYPE, SemanticTokenType.Type)

    override def visitExprTypeUInt(ctx: ExprTypeUIntContext): List[SemanticToken] =
      getTokens(ctx.UINTTYPE, SemanticTokenType.Type)

    override def visitExprTypeSNum(ctx: ExprTypeSNumContext): List[SemanticToken] =
      getTokens(ctx.INT, SemanticTokenType.Type)

    override def visitExprTypeUNum(ctx: ExprTypeUNumContext): List[SemanticToken] =
      getTokens(ctx.UINT, SemanticTokenType.Type)

    override def visitExprTypeVoid(ctx: ExprTypeVoidContext): List[SemanticToken] =
      getTokens(ctx.VOID, SemanticTokenType.Type)

    override def visitExprCall(ctx: ExprCallContext): List[SemanticToken] =
      visit(ctx.expr) ++ parentVisitor(ctx.args)

    override def visitExprIndex(ctx: ExprIndexContext): List[SemanticToken] =
      visit(ctx.expr(0)) ++ parentVisitor(ctx.expr(1))

    override def visitIdent(ctx: IdentContext): List[SemanticToken] =
      parentVisitor(ctx.expr).flatten ++
        getTokens(ctx.IDENTIFIER, SemanticTokenType.Type) ++
        getTokens(ctx.HASH, SemanticTokenType.Operator)

  }

  private def identTokens(ctx: IdentContext, typ: SemanticTokenType.Type): List[SemanticToken] =
    ctx match {
      case null => Nil
      case _ =>
        getTokens(ctx.IDENTIFIER, typ) ++
          getTokens(ctx.HASH, SemanticTokenType.Operator) ++
          visit(ctx.expr).flatten
    }

  override def visitIdent(ctx: IdentContext): List[SemanticToken] =
    identTokens(ctx, SemanticTokenType.Variable)

  override def visitExprTypeBool(ctx: ExprTypeBoolContext): List[SemanticToken] =
    getTokens(ctx.BOOL, SemanticTokenType.Type)

  override def visitExprTypeSInt(ctx: ExprTypeSIntContext): List[SemanticToken] =
    getTokens(ctx.INTTYPE, SemanticTokenType.Type)

  override def visitExprTypeUInt(ctx: ExprTypeUIntContext): List[SemanticToken] =
    getTokens(ctx.UINTTYPE, SemanticTokenType.Type)

  override def visitExprTypeSNum(ctx: ExprTypeSNumContext): List[SemanticToken] =
    getTokens(ctx.INT, SemanticTokenType.Type)

  override def visitExprTypeUNum(ctx: ExprTypeUNumContext): List[SemanticToken] =
    getTokens(ctx.UINT, SemanticTokenType.Type)

  override def visitExprTypeVoid(ctx: ExprTypeVoidContext): List[SemanticToken] =
    getTokens(ctx.VOID, SemanticTokenType.Type)

  override def visitExprLitString(ctx: ExprLitStringContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.String)

  override def visitExprLitTrue(ctx: ExprLitTrueContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.Number)

  override def visitExprLitFalse(ctx: ExprLitFalseContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.Number)

  override def visitExprLitSizedInt(ctx: ExprLitSizedIntContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.Number)

  override def visitExprLitUnsizedInt(ctx: ExprLitUnsizedIntContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.Number)

  override def visitImportOne(ctx: ImportOneContext): List[SemanticToken] =
    getTokens(ctx.IMPORT, SemanticTokenType.Keyword) ++
      getTokens(ctx.STRING, SemanticTokenType.String) ++
      getTokens(ctx.AS, SemanticTokenType.String) ++
      getTokens(ctx.ident, SemanticTokenType.Variable)

  override def visitFromOne(ctx: FromOneContext): List[SemanticToken] =
    getTokens(ctx.FROM, SemanticTokenType.Keyword) ++
      getTokens(ctx.STRING, SemanticTokenType.String) ++
      getTokens(ctx.IMPORT, SemanticTokenType.Keyword) ++
      getTokens(ctx.expr, SemanticTokenType.Variable) ++
      getTokens(ctx.AS, SemanticTokenType.String) ++
      getTokens(ctx.ident, SemanticTokenType.Variable)

  override def visitFromAll(ctx: FromAllContext): List[SemanticToken] =
    getTokens(ctx.FROM, SemanticTokenType.Keyword) ++
      getTokens(ctx.STRING, SemanticTokenType.String) ++
      getTokens(ctx.IMPORT, SemanticTokenType.Keyword) ++
      getTokens(ctx.MUL, SemanticTokenType.Operator)

  override def visitUsingOne(ctx: UsingOneContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.USING, SemanticTokenType.Keyword) ++
      getTokens(ctx.AS, SemanticTokenType.Keyword)

  override def visitUsingAll(ctx: UsingAllContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.USING, SemanticTokenType.Keyword) ++
      getTokens(ctx.MUL, SemanticTokenType.Operator)

  override def visitAssertionAssert(ctx: AssertionAssertContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.ASSERT, SemanticTokenType.Keyword) ++
      getTokens(ctx.STRING, SemanticTokenType.String)

  override def visitAssertionStatic(ctx: AssertionStaticContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.STATIC, SemanticTokenType.Keyword) ++
      getTokens(ctx.ASSERT, SemanticTokenType.Keyword) ++
      getTokens(ctx.STRING, SemanticTokenType.String)

  override def visitAssertionUnreachable(ctx: AssertionUnreachableContext): List[SemanticToken] =
    getTokens(ctx.UNREACHABLE, SemanticTokenType.Keyword) ++
      getTokens(ctx.STRING, SemanticTokenType.String)

  override def visitExprBinary(ctx: ExprBinaryContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.op, SemanticTokenType.Operator)

  override def visitExprUnary(ctx: ExprUnaryContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.op, SemanticTokenType.Operator)

  override def visitStmtIf(ctx: StmtIfContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.IF, SemanticTokenType.Keyword) ++
      getTokens(ctx.ELSE, SemanticTokenType.Keyword)

  override def visitStmtCase(ctx: StmtCaseContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.CASE, SemanticTokenType.Keyword)

  override def visitStmtLoop(ctx: StmtLoopContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.LOOP, SemanticTokenType.Keyword)

  override def visitStmtDo(ctx: StmtDoContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.DO, SemanticTokenType.Keyword) ++
      getTokens(ctx.WHILE, SemanticTokenType.Keyword)

  override def visitStmtWhile(ctx: StmtWhileContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.WHILE, SemanticTokenType.Keyword)

  override def visitStmtFor(ctx: StmtForContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.FOR, SemanticTokenType.Keyword)

  override def visitStmtLet(ctx: StmtLetContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.LET, SemanticTokenType.Keyword)

  override def visitStmtFence(ctx: StmtFenceContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.FENCE, SemanticTokenType.Keyword)

  override def visitStmtBreak(ctx: StmtBreakContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.BREAK, SemanticTokenType.Keyword)

  override def visitStmtContinue(ctx: StmtContinueContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.CONTINUE, SemanticTokenType.Keyword)

  override def visitStmtGoto(ctx: StmtGotoContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.GOTO, SemanticTokenType.Keyword)

  override def visitStmtReturn(ctx: StmtReturnContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.RETURN, SemanticTokenType.Keyword)

  override def visitStmtPost(ctx: StmtPostContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.op, SemanticTokenType.Operator)

  override def visitStmtWait(ctx: StmtWaitContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.WAIT, SemanticTokenType.Keyword)

  override def visitExprKeyword(ctx: ExprKeywordContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.Keyword)

  override def visitExprThis(ctx: ExprThisContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx, SemanticTokenType.Keyword)

  override def visitExprCall(ctx: ExprCallContext): List[SemanticToken] =
    FunctionTokenVisitor(ctx.expr) ++ visit(ctx.args)

  override def visitExprDot(ctx: ExprDotContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.inout, SemanticTokenType.Keyword)

  override def visitExprSlice(ctx: ExprSliceContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.op, SemanticTokenType.Operator)

  override def visitDescVar(ctx: DescVarContext): List[SemanticToken] =
    visit(ctx.init) ++
      TypeTokenVisitor(ctx.expr(0)) ++
      getTokens(ctx.STATIC, SemanticTokenType.Keyword) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Keyword) ++
      identTokens(ctx.ident, SemanticTokenType.Variable)

  override def visitDescIn(ctx: DescInContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      TypeTokenVisitor(ctx.spec) ++
      getTokens(ctx.IN, SemanticTokenType.Keyword) ++
      getTokens(ctx.PIPELINE, SemanticTokenType.Keyword) ++
      visit(ctx.fct)

  override def visitDescOut(ctx: DescOutContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      TypeTokenVisitor(ctx.spec) ++
      getTokens(ctx.OUT, SemanticTokenType.Keyword) ++
      getTokens(ctx.PIPELINE, SemanticTokenType.Keyword) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Operator) ++
      visit(ctx.fct) ++
      visit(ctx.stt) ++
      visit(ctx.init)

  override def visitDescSnoop(ctx: DescSnoopContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      TypeTokenVisitor(ctx.spec) ++
      getTokens(ctx.SNOOP, SemanticTokenType.Keyword) ++
      visit(ctx.fct)

  override def visitDescPipeVar(ctx: DescPipeVarContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      TypeTokenVisitor(ctx.expr) ++
      getTokens(ctx.PIPELINE, SemanticTokenType.Keyword)

  override def visitDescParam(ctx: DescParamContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      TypeTokenVisitor(ctx.expr(0)) ++
      getTokens(ctx.PARAM, SemanticTokenType.Keyword) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Operator) ++
      visit(ctx.init)

  override def visitDescParamType(ctx: DescParamTypeContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      getTokens(ctx.PARAM, SemanticTokenType.Keyword) ++
      getTokens(ctx.TYPE, SemanticTokenType.Keyword) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Operator) ++
      visit(ctx.init)

  override def visitDescConst(ctx: DescConstContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      getTokens(ctx.CONST, SemanticTokenType.Keyword) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Operator) ++
      TypeTokenVisitor(ctx.expr(0)) ++
      visit(ctx.expr(1))

  override def visitDescArr(ctx: DescArrContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Variable) ++
      TypeTokenVisitor(ctx.expr(0)) ++
      visit(ctx.expr(1))

  override def visitDescSram(ctx: DescSramContext): List[SemanticToken] =
    TypeTokenVisitor(ctx.expr(0)) ++
      visit(ctx.expr(1)) ++
      identTokens(ctx.ident, SemanticTokenType.Variable) ++
      getTokens(ctx.SRAM, SemanticTokenType.Keyword) ++
      getTokens(ctx.WIRE, SemanticTokenType.Keyword)

  override def visitDescType(ctx: DescTypeContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Type) ++
      TypeTokenVisitor(ctx.expr) ++
      getTokens(ctx.TYPEDEF, SemanticTokenType.Keyword)

  override def visitDescEntity(ctx: DescEntityContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Class) ++
      getTokens(ctx.entity_keyword, SemanticTokenType.Keyword) ++
      visit(ctx.ent).flatten

  override def visitDescRecord(ctx: DescRecordContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Struct) ++
      getTokens(ctx.STRUCT, SemanticTokenType.Keyword) ++
      visit(ctx.rec).flatten

  override def visitDescInstance(ctx: DescInstanceContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Class) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Operator) ++
      getTokens(ctx.keyword, SemanticTokenType.Keyword) ++
      visit(ctx.expr)

  override def visitDescSingleton(ctx: DescSingletonContext): List[SemanticToken] =
    getTokens(ctx.NEW, SemanticTokenType.Keyword) ++
      getTokens(ctx.entity_keyword, SemanticTokenType.Keyword) ++
      identTokens(ctx.ident, SemanticTokenType.Class) ++
      visit(ctx.ent).flatten

  override def visitDescFuncAlogic(ctx: DescFuncAlogicContext): List[SemanticToken] =
    FunctionTokenVisitor(ctx.ident) ++
      TypeTokenVisitor(ctx.expr) ++
      getTokens(ctx.STATIC, SemanticTokenType.Keyword) ++
      visit(ctx.formal_arguments) ++
      visit(ctx.stmt).flatten

  override def visitDescFuncImport(ctx: DescFuncImportContext): List[SemanticToken] =
    TypeTokenVisitor(ctx.expr) ++
      getTokens(ctx.IMPORT, SemanticTokenType.Keyword) ++
      getTokens(ctx.IDENTIFIER, SemanticTokenType.Function) ++
      visit(ctx.formal_arguments)

  override def visitDescGenIf(ctx: DescGenIfContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Namespace) ++
      ctx.IF.asScala.flatMap(getTokens(_, SemanticTokenType.Keyword)) ++
      ctx.ELSE.asScala.flatMap(getTokens(_, SemanticTokenType.Keyword)) ++
      getTokens(ctx.GEN, SemanticTokenType.Keyword) ++
      visit(ctx.conds).flatten ++
      visit(ctx.thenItemss).flatten ++
      visit(ctx.elseItems)

  override def visitDescGenFor(ctx: DescGenForContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Namespace) ++
      getTokens(ctx.GEN, SemanticTokenType.Keyword) ++
      getTokens(ctx.FOR, SemanticTokenType.Keyword) ++
      visit(ctx.ginits) ++
      visit(ctx.expr) ++
      visit(ctx.lsteps) ++
      visit(ctx.genitems)

  override def visitDescGenRange(ctx: DescGenRangeContext): List[SemanticToken] =
    identTokens(ctx.ident, SemanticTokenType.Namespace) ++
      getTokens(ctx.GEN, SemanticTokenType.Keyword) ++
      getTokens(ctx.FOR, SemanticTokenType.Keyword) ++
      getTokens(ctx.IDENTIFIER, SemanticTokenType.Variable) ++
      getTokens(ctx.op, SemanticTokenType.Operator) ++
      TypeTokenVisitor(ctx.expr(0)) ++
      visit(ctx.expr(1)) ++
      visit(ctx.genitems)

  override def visitFCTSync(ctx: FCTSyncContext): List[SemanticToken] =
    getTokens(ctx.SYNC, SemanticTokenType.Keyword)

  override def visitFCTSyncReady(ctx: FCTSyncReadyContext): List[SemanticToken] = {
    val srLoc = ctx.SYNC_READY.loc
    val endLineOffset = srLoc.source.lineFor(srLoc.end) - srLoc.source.lineFor(srLoc.start)
    List(
      SemanticToken(
        Loc(
          srLoc.file,
          srLoc.line,
          srLoc.source,
          srLoc.start,
          srLoc.start + 4,
          srLoc.point,
          srLoc.trueFileOpt
        ),
        SemanticTokenType.Keyword
      ), // 'sync'
      SemanticToken(
        Loc(
          srLoc.file,
          srLoc.line + endLineOffset,
          srLoc.source,
          srLoc.end - 5,
          srLoc.end,
          srLoc.end - 5,
          srLoc.trueFileOpt
        ),
        SemanticTokenType.Keyword
      ) // 'ready'
    )
  }

  override def visitSTTWire(ctx: STTWireContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.WIRE, SemanticTokenType.Keyword)

  override def visitSlices(ctx: SlicesContext): List[SemanticToken] =
    visitChildren(ctx) ++
      ctx.BSLICE.asScala.flatMap(getTokens(_, SemanticTokenType.Keyword)) ++
      ctx.FSLICE.asScala.flatMap(getTokens(_, SemanticTokenType.Keyword)) ++
      ctx.BUBBLE.asScala.flatMap(getTokens(_, SemanticTokenType.Keyword))

  override def visitFormal_arguments(ctx: Formal_argumentsContext): List[SemanticToken] =
    ctx.expr.asScala.flatMap(TypeTokenVisitor(_)).toList ++
      ctx.IDENTIFIER.asScala.flatMap(getTokens(_, SemanticTokenType.Variable))

  override def visitLoopInitDesc(ctx: LoopInitDescContext): List[SemanticToken] =
    TypeTokenVisitor(ctx.expr(0)) ++
      getTokens(ctx.IDENTIFIER, SemanticTokenType.Variable) ++
      getTokens(ctx.EQUALS, SemanticTokenType.Operator) ++
      visit(ctx.expr(1))

  override def visitGinit(ctx: GinitContext): List[SemanticToken] =
    TypeTokenVisitor(ctx.expr(0)) ++
      getTokens(ctx.IDENTIFIER, SemanticTokenType.Variable) ++
      visit(ctx.expr(1))

  override def visitCaseDefault(ctx: CaseDefaultContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.DEFAULT, SemanticTokenType.Keyword)

  override def visitEntFenceBlock(ctx: EntFenceBlockContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.FENCE, SemanticTokenType.Keyword)

  override def visitEntVerbatimBlock(ctx: EntVerbatimBlockContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.VERBATIM, SemanticTokenType.Keyword)

  override def visitAttributes(ctx: AttributesContext): List[SemanticToken] =
    getTokens(ctx, SemanticTokenType.Comment)

  override def visitEntConnect(ctx: EntConnectContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.point, SemanticTokenType.Operator)

  override def visitEntConnectInputs(ctx: EntConnectInputsContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.point, SemanticTokenType.Operator) ++
      ctx.MUL.asScala.flatMap(getTokens(_, SemanticTokenType.Operator))

  override def visitStmtAssign(ctx: StmtAssignContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.EQUALS, SemanticTokenType.Operator)

  override def visitStmtUpdate(ctx: StmtUpdateContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.ASSIGNOP, SemanticTokenType.Operator)

  override def visitExprTernary(ctx: ExprTernaryContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.QUESTIONMARK, SemanticTokenType.Operator) ++
      getTokens(ctx.COLON, SemanticTokenType.Operator)

  override def visitLoopInitAssign(ctx: LoopInitAssignContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.EQUALS, SemanticTokenType.Operator)

  override def visitLoopStepAssign(ctx: LoopStepAssignContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.EQUALS, SemanticTokenType.Operator)

  override def visitLoopStepUpdate(ctx: LoopStepUpdateContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.ASSIGNOP, SemanticTokenType.Operator)

  override def visitLoopStepPost(ctx: LoopStepPostContext): List[SemanticToken] =
    visitChildren(ctx) ++ getTokens(ctx.op, SemanticTokenType.Operator)

  override def visitPkgCompile(ctx: PkgCompileContext): List[SemanticToken] =
    visitChildren(ctx) ++
      getTokens(ctx.COMPILE, SemanticTokenType.Keyword) ++
      getTokens(ctx.AS, SemanticTokenType.Keyword)

}
