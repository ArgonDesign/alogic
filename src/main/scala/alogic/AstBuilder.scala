package alogic

import scala.collection.mutable

import org.antlr.v4.runtime.tree.RuleNode

import Antlr4Conversions._
import alogic.AstOps._
import alogic.antlr._
import alogic.antlr.VParser._

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
      case Left(t @ Task(_, _, _, _)) => t
    }
  }

  object EntityVisitor extends VBaseVisitor[Either[AlogicAST, Unit]] {
    object TaskContentVisitor extends VBaseVisitor[AlogicAST] {
      override def visitFunction(ctx: FunctionContext) = Function(ctx.IDENTIFIER, ControlBlock(StatementVisitor(ctx.stmts)))
      override def visitFenceFunction(ctx: FenceFunctionContext) = FenceFunction(CombinatorialBlock(StatementVisitor(ctx.stmts)))
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

    override def visitTask(ctx: TaskContext) = {
      val tasktype = ctx.tasktype.text match {
        case "fsm"      => Fsm
        case "pipeline" => Pipeline
        case "verilog"  => Verilog
      }
      Left(Task(tasktype, ctx.IDENTIFIER, DeclVisitor(ctx.decls), TaskContentVisitor(ctx.contents)))
    }

    override def visitNetwork(ctx: NetworkContext) = {
      val contents = visit(ctx.contents) collect { case Left(x) => x; }
      Left(Task(Network, ctx.IDENTIFIER, DeclVisitor(ctx.decls), contents))
    }

    override def visitConnect(ctx: ConnectContext) = {
      val Left(name) = visit(ctx.dotted_name())
      Left(Connect(name, CommaArgsVisitor(ctx.comma_args()))) // TODO check that these names exist?
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
    object LookUpDeclVarRef extends VBaseVisitor[AlogicAST] {
      override def visitVarRefIndex(ctx: VarRefIndexContext) = {
        ArrayLookup(visit(ctx.dotted_name), ExprVisitor(ctx.expr))
      }

      override def visitDotted_name(ctx: Dotted_nameContext) = {
        val name = ctx.es.toList.map(_.text) mkString "."
        DottedName(List(scope(ctx, name)))
      }
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

    override def visitTaskDecl(ctx: TaskDeclContext) = visit(ctx.declaration)

    override def visitDeclNoInit(ctx: DeclNoInitContext) =
      VarDeclaration(TypeVisitor(ctx.known_type()), LookUpDeclVarRef(ctx.var_ref), None)

    override def visitDeclInit(ctx: DeclInitContext) =
      VarDeclaration(TypeVisitor(ctx.known_type()), LookUpDeclVarRef(ctx.var_ref), Some(ExprVisitor(ctx.expr)))

  }

  object VarRefVisitor extends VBaseVisitor[AlogicExpr] {
    override def visitVarRef(ctx: VarRefContext) = visit(ctx.dotted_name)
    override def visitVarRefIndex(ctx: VarRefIndexContext) =
      ArrayLookup(visit(ctx.dotted_name), ExprVisitor(ctx.expr))
    override def visitVarRefSlice(ctx: VarRefSliceContext) =
      BinaryArrayLookup(visit(ctx.dotted_name), ExprVisitor(ctx.expr(0)), ctx.op, ExprVisitor(ctx.expr(1)))

    override def visitLValueCat(ctx: LValueCatContext) = BitCat(visit(ctx.refs))

    override def visitDotted_name(ctx: Dotted_nameContext) = {
      // Look up name in namespace
      val (head :: tail) = ctx.es.toList.map(_.text)
      DottedName(scope(ctx, head) :: tail)
    }
  }

  object ExprVisitor extends VBaseVisitor[AlogicExpr] {
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
    override def visitExprCat(ctx: ExprCatContext) = BitCat(CommaArgsVisitor(ctx.comma_args()))
    override def visitExprVarRef(ctx: ExprVarRefContext) = VarRefVisitor(ctx)
    override def visitExprDollar(ctx: ExprDollarContext) = DollarCall(ctx.DOLLARID, CommaArgsVisitor(ctx.comma_args()))
    override def visitExprTrue(ctx: ExprTrueContext) = Num("1'b1")
    override def visitExprFalse(ctx: ExprFalseContext) = Num("1'b0")
    override def visitExprTrickNum(ctx: ExprTrickNumContext) = Num(ctx.TICKNUM)
    override def visitExprConstTickNum(ctx: ExprConstTickNumContext) = Num(ctx.CONSTANT + ctx.TICKNUM)
    override def visitExprConst(ctx: ExprConstContext) = Num(ctx.CONSTANT)
    override def visitExprLiteral(ctx: ExprLiteralContext) = Literal(ctx.LITERAL)

    override def visitExprCall(ctx: ExprCallContext) = {
      val n = VarRefVisitor(ctx.dotted_name)
      val a = CommaArgsVisitor(ctx.comma_args())
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
        case _ => FunCall(n, a)
      }
    }
  }

  object StatementVisitor extends VBaseVisitor[AlogicAST] {
    override def visitBlockStmt(ctx: BlockStmtContext) = visit(ctx.stmts) match {
      case s if (s.length > 0 && is_control_stmt(s.last)) => ControlBlock(s)
      case s if (!s.exists(is_control_stmt))              => CombinatorialBlock(s)
      case s => {
        Message.error(ctx, "A control block must end with a control statement");
        ControlBlock(s)
      }
    }

    override def visitDeclStmt(ctx: DeclStmtContext) = DeclVisitor(ctx.declaration()) match {
      case s: VarDeclaration => DeclarationStmt(s)
      case _ => {
        Message.error(ctx, "Only variable declarations allowed inside functions")
        DeclarationStmt(VarDeclaration(State, DottedName(List("Unknown")), None))
      }
    }

    override def visitWhileStmt(ctx: WhileStmtContext) = {
      val cond = ExprVisitor(ctx.expr)
      val body = visit(ctx.stmts)
      if (!is_control_stmt(body.last)) {
        // TODO(geza): can't we just infer a fence here, so all loops have the same rule
        // and the while is not special?
        Message.error(ctx, "The body of a while loop must end with a control statement")
      }
      ControlWhile(cond, body)
    }

    override def visitIfStmt(ctx: IfStmtContext) = {
      val cond = ExprVisitor(ctx.expr())
      val yes = visit(ctx.thenStmt)
      val no = visit(Option(ctx.elseStmt))

      (yes, no) match {
        case (y, None) if (is_control_stmt(y))                            => ControlIf(cond, yes, None)
        case (y, Some(n)) if (is_control_stmt(y) && is_control_stmt(n))   => ControlIf(cond, yes, no)
        case (y, None) if (!is_control_stmt(y))                           => CombinatorialIf(cond, yes, None)
        case (y, Some(n)) if (!is_control_stmt(y) && !is_control_stmt(n)) => CombinatorialIf(cond, yes, no)
        case _ => {
          Message.error(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements");
          ControlIf(cond, yes, no)
        }
      }
    }

    override def visitCaseStmt(ctx: CaseStmtContext) = {

      object CaseVisitor extends VBaseVisitor[AlogicAST] {
        override def visitDefaultCase(ctx: DefaultCaseContext) = {
          val s = StatementVisitor(ctx.statement())
          if (is_control_stmt(s))
            ControlCaseLabel(List(), s)
          else
            CombinatorialCaseLabel(List(), s)
        }

        override def visitNormalCase(ctx: NormalCaseContext) = {
          val s = StatementVisitor(ctx.statement())
          val args = CommaArgsVisitor(ctx.comma_args())
          if (is_control_stmt(s))
            ControlCaseLabel(args, s)
          else
            CombinatorialCaseLabel(args, s)
        }
      }

      def is_control_label(cmd: AlogicAST): Boolean = cmd match {
        case ControlCaseLabel(_, _) => true
        case _                      => false
      }

      val test = ExprVisitor(ctx.expr())
      CaseVisitor(ctx.cases) match {
        case stmts if (stmts.forall(is_control_label))  => ControlCaseStmt(test, stmts)
        case stmts if (!stmts.exists(is_control_label)) => CombinatorialCaseStmt(test, stmts)
        case stmts => {
          Message.error(ctx, "Either all or none of the case items must be control statements");
          ControlCaseStmt(test, stmts)
        }
      }
    }

    object ForInitVisitor extends VBaseVisitor[(Option[VarDeclaration], AlogicAST)] {
      override def visitForInitNoDecl(ctx: ForInitNoDeclContext) = (None, StatementVisitor(ctx.assignment_statement))
      override def visitDeclInit(ctx: DeclInitContext) = {
        val varDecl = DeclVisitor(ctx).asInstanceOf[VarDeclaration]
        val initExpr = Assign(VarRefVisitor(ctx.var_ref), ExprVisitor(ctx.expr))
        (Some(varDecl), initExpr)
      }
    }

    override def visitForStmt(ctx: ForStmtContext) = {
      val (optDecl, initExpr) = ForInitVisitor(ctx.init)
      val forAST = ControlFor(initExpr, ExprVisitor(ctx.cond), visit(ctx.step), visit(ctx.stmts))
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

    override def visitAssignInc(ctx: AssignIncContext) = Plusplus(VarRefVisitor(ctx.lvalue))
    override def visitAssignDec(ctx: AssignDecContext) = Minusminus(VarRefVisitor(ctx.lvalue))
    override def visitAssign(ctx: AssignContext) = Assign(VarRefVisitor(ctx.lvalue), ExprVisitor(ctx.expr()))
    override def visitAssignUpdate(ctx: AssignUpdateContext) = Update(VarRefVisitor(ctx.lvalue), ctx.ASSIGNOP, ExprVisitor(ctx.expr()))

    override def visitExprStmt(ctx: ExprStmtContext) = ExprVisitor(ctx.expr)
  }

  object CommaArgsVisitor extends VBaseVisitor[List[AlogicExpr]] {
    override def visitComma_args(ctx: Comma_argsContext) = ExprVisitor(ctx.es)
  }

  object TypeVisitor extends VBaseVisitor[AlogicType] {
    override def visitBoolType(ctx: BoolTypeContext) = IntType(false, 1)
    override def visitIntType(ctx: IntTypeContext) = IntType(true, ctx.INTTYPE.text.tail.toInt)
    override def visitUintType(ctx: UintTypeContext) = IntType(false, ctx.UINTTYPE.text.tail.toInt)
    object FieldVisitor extends VBaseVisitor[FieldType] {
      override def visitField(ctx: FieldContext) = Field(TypeVisitor(ctx.known_type()), ctx.IDENTIFIER)
    }
    override def visitStructType(ctx: StructTypeContext) = Struct(FieldVisitor(ctx.fields))
    override def visitIntVType(ctx: IntVTypeContext) = IntVType(true, CommaArgsVisitor(ctx.comma_args()))
    override def visitUintVType(ctx: UintVTypeContext) = IntVType(false, CommaArgsVisitor(ctx.comma_args()))
    override def visitIdentifierType(ctx: IdentifierTypeContext) = {
      val s = ctx.IDENTIFIER.text
      typedefs.getOrElse(s, {
        Message.error(ctx, s"Unknown type '$s'")
        IntType(false, 1)
      })
    }
  }

  // Build the abstract syntax tree from a parse tree
  def apply(parseTree: RuleNode): Program = {
    // Extract names from declarations and build scopes
    scope = new VScope(parseTree)
    // Then build abstract syntax tree and remap identifiers
    Program(ProgVisitor(parseTree))
  }
}
