package alogic.ast

import alogic.antlr.VParser._

import scala.collection.immutable.ListMap
import scala.collection.mutable

import org.antlr.v4.runtime.tree.RuleNode

import alogic.Antlr4Conversions._
import alogic.Message
import alogic.VBaseVisitor
import alogic.VScope
import alogic.ast.AstOps._
import org.antlr.v4.runtime.ParserRuleContext

// The aim of the AstBuilder stage is:
//   Build an abstract syntax tree
//   Deal with as many error conditions as possible (while we can easily report error location)
//   Deal with typedefs
//   Deal with variable scope
//   Rewrite go/zxt/sxt/read/write/lock/unlock function calls
//
// We use different visitors for the different things we wish to extract.
//
// The overall API is to make a AstBuilder class, and then call it to build the ast from a parse tree.
// The object can be called multiple times to allow us to only build a common header file once.
//

class AstBuilder {

  private[this] var scope: VScope = null
  private[this] val typedefs = mutable.Map[String, AlogicType]()

  // Keep track of containing variable scope for each parse tree node

  typedefs("state") = State

  object ProgVisitor extends VBaseVisitor[List[Task]] {
    override def visitStart(ctx: StartContext) = EntityVisitor(ctx.entities) collect {
      case Left(t: Task) => t
    }
  }

  object EntityVisitor extends VBaseVisitor[Either[Node, Unit]] {

    object TaskContentVisitor extends VBaseVisitor[Node] {
      override def visitFunction(ctx: FunctionContext) = Function(ctx.IDENTIFIER, ControlBlock(StatementVisitor(ctx.stmts)))
      override def visitFenceFunction(ctx: FenceFunctionContext) = {
        val body = StatementVisitor(ctx.stmts) collect {
          case stmt: CombStmt => stmt
          case stmt: CtrlStmt => Message.fatal(ctx, "Body of 'fence' function must not contain control statements")
        }
        FenceFunction(CombinatorialBlock(body))
      }
      override def visitVerilogFunction(ctx: VerilogFunctionContext) = VerilogFunction(ctx.VERILOGBODY.text.drop(1).dropRight(1))
    }

    override def visitTypedef(ctx: TypedefContext) = {
      val s = ctx.IDENTIFIER.text
      if (typedefs contains s) {
        Message.error(ctx, s"Repeated typedef '$s'")
      } else {
        typedefs(s) = TypeVisitor(ctx.known_type())
      }
      Right(())
    }

    override def visitTaskFSM(ctx: TaskFSMContext) = {
      val contents = TaskContentVisitor(ctx.contents)
      val fns = contents collect { case x: Function => x }
      val fencefns = contents collect { case x: FenceFunction => x }
      val vfns = contents collect { case x: VerilogFunction => x }

      // TODO: check there is only 1 fence function and olny in fsm
      Left(FsmTask(ctx.IDENTIFIER, DeclVisitor(ctx.decls), fns, fencefns.headOption, vfns))
    }

    override def visitTaskVerilog(ctx: TaskVerilogContext) = {
      val vfns = TaskContentVisitor(ctx.contents) collect { case x: VerilogFunction => x }
      Left(VerilogTask(ctx.IDENTIFIER, DeclVisitor(ctx.decls), vfns))
    }

    override def visitNetwork(ctx: NetworkContext) = {
      val contents = visit(ctx.contents) collect { case Left(x) => x; }
      Left(NetworkTask(ctx.IDENTIFIER, DeclVisitor(ctx.decls), contents))
    }

    override def visitConnect(ctx: ConnectContext) = {
      val Left(name) = visit(ctx.dotted_name())
      Left(Connect(name, ExprVisitor(ctx.commaexpr))) // TODO check that these names exist?
    }

    object ParamArgsVisitor extends VBaseVisitor[List[Assign]] {
      object ParamAssignVisitor extends VBaseVisitor[Assign] {
        override def visitParamAssign(ctx: ParamAssignContext) = Assign(ExprVisitor(ctx.expr(0)), ExprVisitor(ctx.expr(1)))
      }

      override def visitParam_args(ctx: Param_argsContext) = ParamAssignVisitor(ctx.es)
    }

    override def visitInstantiate(ctx: InstantiateContext) = {
      Left(Instantiate(ctx.IDENTIFIER(0), ctx.IDENTIFIER(1), ParamArgsVisitor(ctx.param_args())))
    }
  }

  object DeclVisitor extends VBaseVisitor[Declaration] {
    // Extract name from var_ref and look up in current scope
    object LookUpDeclVarRef extends VBaseVisitor[VarRef] {
      object LookUpDottedName extends VBaseVisitor[DottedName] {
        override def visitDotted_name(ctx: Dotted_nameContext) = {
          val name = ctx.es.toList.map(_.text) mkString "."
          DottedName(List(scope(ctx, name)))
        }
      }

      override def visitVarRefIndex(ctx: VarRefIndexContext) =
        ArrayLookup(LookUpDottedName(ctx.dotted_name), ExprVisitor(ctx.es))
      override def visitDotted_name(ctx: Dotted_nameContext) =
        LookUpDottedName(ctx)
    }

    object SyncTypeVisitor extends VBaseVisitor[SyncType] {
      override def visitSyncReadyBubbleType(ctx: SyncReadyBubbleTypeContext) = SyncReadyBubble
      override def visitWireSyncAcceptType(ctx: WireSyncAcceptTypeContext) = WireSyncAccept
      override def visitSyncReadyType(ctx: SyncReadyTypeContext) = SyncReady
      override def visitWireSyncType(ctx: WireSyncTypeContext) = WireSync
      override def visitSyncAcceptType(ctx: SyncAcceptTypeContext) = SyncAccept
      override def visitSyncType(ctx: SyncTypeContext) = Sync
      override def visitWireType(ctx: WireTypeContext) = Wire
      override val defaultResult = Wire
    }

    override def visitTaskDeclOut(ctx: TaskDeclOutContext) =
      OutDeclaration(SyncTypeVisitor(ctx.sync_type), TypeVisitor(ctx.known_type), scope(ctx, ctx.IDENTIFIER))

    override def visitTaskDeclIn(ctx: TaskDeclInContext) =
      InDeclaration(SyncTypeVisitor(ctx.sync_type), TypeVisitor(ctx.known_type), scope(ctx, ctx.IDENTIFIER))

    override def visitTaskDeclConst(ctx: TaskDeclConstContext) =
      ConstDeclaration(TypeVisitor(ctx.known_type), scope(ctx, ctx.IDENTIFIER), ExprVisitor(ctx.expr))

    override def visitTaskDeclParam(ctx: TaskDeclParamContext) =
      ParamDeclaration(TypeVisitor(ctx.known_type), scope(ctx, ctx.IDENTIFIER), ExprVisitor(ctx.expr))

    override def visitTaskDeclVerilog(ctx: TaskDeclVerilogContext) =
      VerilogDeclaration(TypeVisitor(ctx.known_type()), LookUpDeclVarRef(ctx.var_ref))

    override def visitTaskDecl(ctx: TaskDeclContext) = visit(ctx.decl)

    override def visitDeclNoInit(ctx: DeclNoInitContext) =
      VarDeclaration(TypeVisitor(ctx.known_type()), LookUpDeclVarRef(ctx.var_ref), None)

    override def visitDeclInit(ctx: DeclInitContext) =
      VarDeclaration(TypeVisitor(ctx.known_type()), LookUpDeclVarRef(ctx.var_ref), Some(ExprVisitor(ctx.expr)))

  }

  object LookUpName extends VBaseVisitor[DottedName] {
    override def visitDotted_name(ctx: Dotted_nameContext) = {
      // Look up name in namespace
      val (head :: tail) = ctx.es.toList.map(_.text)
      DottedName(scope(ctx, head) :: tail)
    }
  }

  object VarRefVisitor extends VBaseVisitor[VarRef] {
    override def visitVarRef(ctx: VarRefContext) =
      LookUpName(ctx.dotted_name)
    override def visitVarRefIndex(ctx: VarRefIndexContext) =
      ArrayLookup(LookUpName(ctx.dotted_name), ExprVisitor(ctx.es))
  }

  object LValueVisitor extends VBaseVisitor[Expr] {
    override def visitLValue(ctx: LValueContext) = VarRefVisitor(ctx.var_ref)
    override def visitLValueSlice(ctx: LValueSliceContext) =
      Slice(VarRefVisitor(ctx.var_ref), ExprVisitor(ctx.expr(0)), ctx.op, ExprVisitor(ctx.expr(1)))
    override def visitLValueCat(ctx: LValueCatContext) =
      BitCat(visit(ctx.refs))
  }

  object ExprVisitor extends VBaseVisitor[Expr] {
    // If applied to a commaexpr node, return a list of the constructed expressions
    def apply(ctx: CommaexprContext): List[Expr] = visit(ctx.expr)

    override def visitExprBracket(ctx: ExprBracketContext) = Bracket(visit(ctx.expr))
    override def visitExprUnary(ctx: ExprUnaryContext) = UnaryOp(ctx.op, visit(ctx.expr))
    override def visitExprMulDiv(ctx: ExprMulDivContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprAddSub(ctx: ExprAddSubContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprShift(ctx: ExprShiftContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprCompare(ctx: ExprCompareContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprEqual(ctx: ExprEqualContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprBAnd(ctx: ExprBAndContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprBXor(ctx: ExprBXorContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprBOr(ctx: ExprBOrContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprAnd(ctx: ExprAndContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprOr(ctx: ExprOrContext) = BinaryOp(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprTernary(ctx: ExprTernaryContext) = TernaryOp(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2)))
    override def visitExprRep(ctx: ExprRepContext) = BitRep(visit(ctx.expr(0)), visit(ctx.expr(1)))
    override def visitExprCat(ctx: ExprCatContext) = BitCat(ExprVisitor(ctx.commaexpr))
    override def visitExprVarRef(ctx: ExprVarRefContext) = VarRefVisitor(ctx)
    override def visitExprSlice(ctx: ExprSliceContext) = Slice(VarRefVisitor(ctx.var_ref), visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1)))
    override def visitExprDollar(ctx: ExprDollarContext) = DollarCall(ctx.DOLLARID, ExprVisitor(ctx.commaexpr))
    override def visitExprTrue(ctx: ExprTrueContext) = Num("1'b1")
    override def visitExprFalse(ctx: ExprFalseContext) = Num("1'b0")
    override def visitExprTrickNum(ctx: ExprTrickNumContext) = Num(ctx.TICKNUM)
    override def visitExprConstTickNum(ctx: ExprConstTickNumContext) = Num(ctx.CONSTANT + ctx.TICKNUM)
    override def visitExprConst(ctx: ExprConstContext) = Num(ctx.CONSTANT)
    override def visitExprLiteral(ctx: ExprLiteralContext) = Literal(ctx.LITERAL)

    override def visitExprCall(ctx: ExprCallContext) = {
      val n = LookUpName(ctx.dotted_name)
      val a = ExprVisitor(ctx.commaexpr)
      n match {
        case DottedName(names) if (names.last == "read") => {
          if (a.length > 0)
            Message.error(ctx, s"Interface read takes no arguments (${a.length} found)")
          ReadCall(DottedName(names.init))
        }
        case DottedName(names) if (names.last == "lock") => {
          if (a.length > 0)
            Message.error(ctx, s"Interface lock takes no arguments (${a.length} found)")
          LockCall(DottedName(names.init))
        }
        case DottedName(names) if (names.last == "unlock") => {
          if (a.length > 0)
            Message.error(ctx, s"Interface unlock takes no arguments (${a.length} found)")
          UnlockCall(DottedName(names.init))
        }
        case DottedName(names) if (names.last == "valid" || names.last == "v") => {
          if (a.length > 0)
            Message.error(ctx, s"Accessing valid property takes no arguments (${a.length} found)")
          ValidCall(DottedName(names.init))
        }
        case DottedName(names) if (names.last == "write") => {
          if (a.length != 1)
            Message.error(ctx, s"Interface write takes exactly one argument (${a.length} found)")
          WriteCall(DottedName(names.init), a)
        }
        case DottedName("zxt" :: Nil) => {
          if (a.length != 2)
            Message.error(ctx, s"Zero extend function takes exactly two arguments: number of bits and expression (${a.length} found)")
          Zxt(a(0), a(1))
        }
        case DottedName("sxt" :: Nil) => {
          if (a.length != 2)
            Message.error(ctx, s"Sign extend function takes exactly two arguments: number of bits and expression (${a.length} found)")
          Sxt(a(0), a(1))
        }
        case _ => {
          if (a.length > 0)
            Message.error(ctx, s"State functions take no arguments (${a.length} found)")
          CallExpr(n, a)
        }
      }
    }
  }

  object StatementVisitor extends VBaseVisitor[Stmt] {
    override def visitBlockStmt(ctx: BlockStmtContext) = {
      val stmts = visit(ctx.stmts)
      val ctrlStmts = stmts collect { case s: CtrlStmt => s }
      val combStmts = stmts collect { case s: CombStmt => s }

      (ctrlStmts, combStmts) match { // TODO: Not sure this is the best way to write this
        case (Nil, comb) => CombinatorialBlock(comb)
        case (ctrl, comb) => {
          stmts.last match {
            case s: CtrlStmt => ControlBlock(stmts)
            case s: CombStmt => {
              Message.error(ctx, "A control block must end with a control statement")
              ControlBlock(Nil)
            }
          }
        }
      }
    }

    override def visitDeclStmt(ctx: DeclStmtContext) = DeclVisitor(ctx.decl()) match {
      case s: VarDeclaration => DeclarationStmt(s)
      case _ => {
        Message.error(ctx, "Only variable declarations allowed inside functions")
        DeclarationStmt(VarDeclaration(State, DottedName(List("Unknown")), None))
      }
    }

    override def visitWhileStmt(ctx: WhileStmtContext) = {
      val cond = ExprVisitor(ctx.expr)
      val body = visit(ctx.stmts)
      body.last match {
        case _: CombStmt => Message.error(ctx, "The body of a while loop must end with a control statement")
        case _           =>
      }
      ControlWhile(cond, body)
    }

    override def visitIfStmt(ctx: IfStmtContext) = {
      val cond = ExprVisitor(ctx.expr())
      val thenStmt = visit(ctx.thenStmt)
      val elseStmt = visit(Option(ctx.elseStmt))

      (thenStmt, elseStmt) match {
        case (t: CtrlStmt, None)              => ControlIf(cond, t, None)
        case (t: CtrlStmt, Some(e: CtrlStmt)) => ControlIf(cond, t, Some(e))
        case (t: CombStmt, None)              => CombinatorialIf(cond, t, None)
        case (t: CombStmt, Some(e: CombStmt)) => CombinatorialIf(cond, t, Some(e))
        case _ => {
          Message.error(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements");
          ControlIf(cond, ControlBlock(Nil), None)
        }
      }
    }

    override def visitCaseStmt(ctx: CaseStmtContext) = {

      object DefaultVisitor extends VBaseVisitor[Option[Stmt]] {
        override val defaultResult = None
        override def visitDefaultCase(ctx: DefaultCaseContext) = Some(StatementVisitor(ctx.statement()))
      }

      object CaseVisitor extends VBaseVisitor[Option[Node]] {
        override val defaultResult = None
        override def visitNormalCase(ctx: NormalCaseContext) = {
          val args = ExprVisitor(ctx.commaexpr)
          StatementVisitor(ctx.statement()) match {
            case s: CtrlStmt => Some(ControlCaseLabel(args, s))
            case s: CombStmt => Some(CombinatorialCaseLabel(args, s))
          }
        }
      }

      val test = ExprVisitor(ctx.expr())

      val defaultCase = DefaultVisitor(ctx.cases).flatten match {
        case Nil      => None
        case d :: Nil => Some(d)
        case _        => Message.error(ctx, "More than one 'default' case item specified"); None
      }

      val cases = CaseVisitor(ctx.cases).flatten
      val ctrlCases = cases collect { case s: ControlCaseLabel => s }
      val combCases = cases collect { case s: CombinatorialCaseLabel => s }

      (ctrlCases, combCases, defaultCase) match {
        case (Nil, Nil, None)               => CombinatorialCaseStmt(test, Nil, None)
        case (Nil, Nil, Some(d: CombStmt))  => CombinatorialCaseStmt(test, Nil, Some(d))
        case (Nil, Nil, Some(d: CtrlStmt))  => ControlCaseStmt(test, Nil, Some(d))
        case (Nil, comb, None)              => CombinatorialCaseStmt(test, comb, None)
        case (Nil, comb, Some(d: CombStmt)) => CombinatorialCaseStmt(test, comb, Some(d))
        case (ctrl, Nil, None)              => ControlCaseStmt(test, ctrl, None)
        case (ctrl, Nil, Some(d: CtrlStmt)) => ControlCaseStmt(test, ctrl, Some(d))
        case _ => {
          Message.error(ctx, "Either all or none of the case items must be control statements");
          ControlCaseStmt(test, Nil, None)
        }
      }
    }

    object ForInitVisitor extends VBaseVisitor[(Option[VarDeclaration], CombStmt)] {
      override def visitForInitNoDecl(ctx: ForInitNoDeclContext) = {
        val stmt = StatementVisitor(ctx.assignment_statement) match {
          case s: CombStmt => s
          case _: CtrlStmt => Message.fatal("Unreachable")
        }
        (None, stmt)
      }
      override def visitDeclInit(ctx: DeclInitContext) = {
        val varDecl = DeclVisitor(ctx).asInstanceOf[VarDeclaration]
        val initExpr = Assign(VarRefVisitor(ctx.var_ref), ExprVisitor(ctx.expr))
        (Some(varDecl), initExpr)
      }
    }

    override def visitForStmt(ctx: ForStmtContext) = {
      val (optDecl, initStmt) = ForInitVisitor(ctx.init)
      val stepStmt = visit(ctx.step) match {
        case s: CombStmt => s
        case _: CtrlStmt => Message.fatal("Unreachable")
      }
      val forAST = ControlFor(initStmt, ExprVisitor(ctx.cond), stepStmt, visit(ctx.stmts))
      optDecl match {
        case None       => forAST
        case Some(decl) => ControlBlock(DeclarationStmt(decl) :: forAST :: Nil)
      }
    }

    override def visitDoStmt(ctx: DoStmtContext) = ControlDo(ExprVisitor(ctx.expr), visit(ctx.stmts))

    override def visitFenceStmt(ctx: FenceStmtContext) = FenceStmt
    override def visitBreakStmt(ctx: BreakStmtContext) = BreakStmt
    override def visitReturnStmt(ctx: ReturnStmtContext) = ReturnStmt
    override def visitDollarCommentStmt(ctx: DollarCommentStmtContext) = AlogicComment(ctx.LITERAL)
    override def visitGotoStmt(ctx: GotoStmtContext) = GotoStmt(ctx.IDENTIFIER)

    override def visitAssignmentStmt(ctx: AssignmentStmtContext) = visit(ctx.assignment_statement)

    override def visitAssignInc(ctx: AssignIncContext) = Plusplus(LValueVisitor(ctx.lvalue))
    override def visitAssignDec(ctx: AssignDecContext) = Minusminus(LValueVisitor(ctx.lvalue))
    override def visitAssign(ctx: AssignContext) = Assign(LValueVisitor(ctx.lvalue), ExprVisitor(ctx.expr()))
    override def visitAssignUpdate(ctx: AssignUpdateContext) = Update(LValueVisitor(ctx.lvalue), ctx.ASSIGNOP, ExprVisitor(ctx.expr()))

    override def visitExprStmt(ctx: ExprStmtContext) = ExprVisitor(ctx.expr) match {
      case CallExpr(DottedName(target :: xs), args) => {
        if (!args.isEmpty) {
          Message.fatal(ctx, "Function calls in statement position take no arguments")
        }
        if (!xs.isEmpty) {
          Message.fatal(ctx, "Function calls in statement position must use unqualified name")
        }
        CallStmt(target)
      }
      case expr => ExprStmt(expr)
    }
  }

  object TypeVisitor extends VBaseVisitor[AlogicType] {
    override def visitBoolType(ctx: BoolTypeContext) = IntType(false, 1)
    override def visitIntType(ctx: IntTypeContext) = IntType(true, ctx.INTTYPE.text.tail.toInt)
    override def visitUintType(ctx: UintTypeContext) = IntType(false, ctx.UINTTYPE.text.tail.toInt)
    override def visitStructType(ctx: StructTypeContext) = {
      val pairs = ctx.fields.toList map { c => c.IDENTIFIER.text -> TypeVisitor(c.known_type) }
      Struct(ListMap(pairs: _*))
    }
    override def visitIntVType(ctx: IntVTypeContext) = IntVType(true, ExprVisitor(ctx.commaexpr))
    override def visitUintVType(ctx: UintVTypeContext) = IntVType(false, ExprVisitor(ctx.commaexpr))
    override def visitIdentifierType(ctx: IdentifierTypeContext) = {
      val s = ctx.IDENTIFIER.text
      typedefs.getOrElse(s, {
        Message.error(ctx, s"Unknown type '$s'")
        IntType(false, 1)
      })
    }
  }

  // Build the abstract syntax tree from a parse tree
  def apply(parseTree: ParserRuleContext): List[Task] = {
    // Extract names from declarations and build scopes
    scope = new VScope(parseTree)
    // Then build abstract syntax tree and remap identifiers
    ProgVisitor(parseTree)
  }
}
