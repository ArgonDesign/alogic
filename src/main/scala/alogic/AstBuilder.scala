package alogic

import alogic.antlr._
import alogic.antlr.VParser._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.ParserRuleContext
import scala.collection._
import scala.collection.mutable.ListBuffer
import alogic.AstOps._

import Antlr4Conversions._
import org.antlr.v4.runtime.tree.RuleNode

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
  class BaseVisitor[T] extends VParserBaseVisitor[T] {
    override def visit(tree: ParseTree): T = {
      if (null == tree) defaultResult else super.visit(tree)
    } ensuring (null != _)

    def apply[U <: RuleNode](ctx: U): T = visit(ctx)

    def apply[U <: RuleNode](ctxList: List[U]): List[T] = ctxList.map(apply(_))

    def apply[U <: RuleNode](ctxList: java.util.List[U]): List[T] = apply(ctxList.toList)

    def apply[U <: RuleNode](ctxOpt: Option[U]): Option[T] = ctxOpt.map(apply(_))

    def visit[U <: RuleNode](ctxList: List[U]): List[T] = apply(ctxList)

    def visit[U <: RuleNode](ctxList: java.util.List[U]): List[T] = apply(ctxList)

    def visit[U <: RuleNode](ctxOpt: Option[U]): Option[T] = apply(ctxOpt)
  }

  private[this] val typedefs = mutable.Map[String, AlogicType]()
  private[this] val NS = new Namespace()

  typedefs("state") = State

  // Convert identifier to tree
  private[this] def identifier(ident: String) = DottedName(List(ident))

  object FunVisitor extends BaseVisitor[Unit] {
    override def defaultResult = ()
    override def visitFunction(ctx: FunctionContext): Unit = NS.insert(ctx, ctx.IDENTIFIER)
  }

  object ProgVisitor extends BaseVisitor[List[Task]] {
    override def visitStart(ctx: StartContext) = EntityVisitor(ctx.entities) collect {
      case t @ Task(_, _, _, _) => t
    }
  }

  object EntityVisitor extends BaseVisitor[AlogicAST] {
    override def visitTypedef(ctx: TypedefContext) = {
      val s = ctx.IDENTIFIER.text
      if (typedefs contains s) {
        Message.error(ctx, s"Repeated typedef '$s'")
      } else {
        typedefs(s) = TypeVisitor(ctx.known_type())
      }
      Typedef
    }

    override def visitTask(ctx: TaskContext) = {
      Task(
        TasktypeVisitor(ctx.tasktype),
        ctx.IDENTIFIER,
        DeclVisitor(ctx.decls),
        TaskContentVisitor(ctx.contents))
    }

    override def visitNetwork(ctx: NetworkContext) = {
      Task(
        Network,
        ctx.IDENTIFIER,
        DeclVisitor(ctx.decls),
        visit(ctx.contents))
    }

    override def visitConnect(ctx: ConnectContext) = {
      Connect(visit(ctx.dotted_name()), CommaArgsVisitor(ctx.comma_args())) // TODO check that these names exist?
    }

    object ParamArgsVisitor extends BaseVisitor[List[Assign]] {
      object ParamAssignVisitor extends BaseVisitor[Assign] {
        override def visitParamAssign(ctx: ParamAssignContext) = Assign(ExprVisitor(ctx.expr(0)), ExprVisitor(ctx.expr(1)))
      }

      override def visitParam_args(ctx: Param_argsContext) = ParamAssignVisitor(ctx.es)
    }

    override def visitInstantiate(ctx: InstantiateContext) = {
      Instantiate(ctx.IDENTIFIER(0), ctx.IDENTIFIER(1), ParamArgsVisitor(ctx.param_args()))
    }
  }

  object TaskContentVisitor extends BaseVisitor[AlogicAST] {
    override def visitFunction(ctx: FunctionContext) = Function(ctx.IDENTIFIER, StatementVisitor(ctx.statement()))
    override def visitFenceFunction(ctx: FenceFunctionContext) = FenceFunction(StatementVisitor(ctx.statement()))
    override def visitVerilogFunction(ctx: VerilogFunctionContext) = VerilogFunction(VerilogBodyVisitor(ctx.verilogbody()))
  }

  object VerilogBodyVisitor extends BaseVisitor[String] {
    override def visitVerilogbody(ctx: VerilogbodyContext) = visit(ctx.tks).mkString
    override def visitVany(ctx: VanyContext) = ctx.VANY
    override def visitVbody(ctx: VbodyContext) = visit(ctx.verilogbody())
  }

  object DeclVisitor extends BaseVisitor[Declaration] {
    override def visitOutDecl(ctx: OutDeclContext) = OutDeclaration(
      SyncTypeVisitor(ctx.sync_type()),
      TypeVisitor(ctx.known_type()),
      NS.insert(ctx, ctx.IDENTIFIER))

    override def visitInDecl(ctx: InDeclContext) = InDeclaration(
      SyncTypeVisitor(ctx.sync_type()),
      TypeVisitor(ctx.known_type()),
      NS.insert(ctx, ctx.IDENTIFIER))

    override def visitParamDecl(ctx: ParamDeclContext) = ParamDeclaration(
      TypeVisitor(ctx.known_type()),
      NS.insert(ctx, ctx.IDENTIFIER),
      ExprVisitor(Option(ctx.initializer())))

    override def visitDecl(ctx: DeclContext) = visit(ctx.declaration())

    override def visitVerilogDecl(ctx: VerilogDeclContext) =
      VerilogDeclaration(TypeVisitor(ctx.known_type()), InsertVarVisitor(ctx.var_ref))

    override def visitDeclaration(ctx: DeclarationContext) = VarDeclaration(
      TypeVisitor(ctx.known_type()),
      InsertVarVisitor(ctx.var_ref),
      ExprVisitor(Option(ctx.initializer())) // Insert into NS - tricky because we want to rewrite in visitor - but the identifier is currently unknown
      // We could do all of this as a second stage:
      //   + Stand alone code
      //   - Lose access to ctx for error messages
      //   - an extra pass may be slightly less efficient and produce error messages in a less useful order?
      //
      // Could add a ctx to all of these objects for use in later messages?
      //
      // If in the case class,
      //    - all grow an extra field - a bit ugly
      // If in the base class,
      //    - need an extra Positioned call to insert the ctx?
      //
      // Or we could call a namespace function to indicate we are inside a declaration?
      //    - a bit hacky
      // Or we could use an alternative visitor function to parse declarations?  (Would only need a couple of special cases)
      //   This is the chosen approach
      )
  }

  // This visitor is used to parse an expression used as a declaration of a variable
  // When we have identified the name, we insert it into the namespace
  object InsertVarVisitor extends BaseVisitor[AlogicAST] {
    override def visitVarRefIndex(ctx: VarRefIndexContext) = ArrayLookup(visit(ctx.dotted_name), ExprVisitor(ctx.expr))

    override def visitDotted_name(ctx: Dotted_nameContext) = {
      val s = ctx.es.toList.map(_.text)
      if (s.length != 1)
        Message.error(ctx, s"Malformed declaration '$s'")
      DottedName(List(NS.insert(ctx, s.head)))
    }
  }

  object TasktypeVisitor extends BaseVisitor[TaskType] {
    override def visitFsmType(ctx: FsmTypeContext) = Fsm
    override def visitPipelineType(ctx: PipelineTypeContext) = Pipeline
    override def visitVerilogType(ctx: VerilogTypeContext) = Verilog
  }

  object SyncTypeVisitor extends BaseVisitor[SyncType] {
    override def visitSyncReadyBubbleType(ctx: SyncReadyBubbleTypeContext) = SyncReadyBubble
    override def visitWireSyncAcceptType(ctx: WireSyncAcceptTypeContext) = WireSyncAccept
    override def visitSyncReadyType(ctx: SyncReadyTypeContext) = SyncReady
    override def visitWireSyncType(ctx: WireSyncTypeContext) = WireSync
    override def visitSyncAcceptType(ctx: SyncAcceptTypeContext) = SyncAccept
    override def visitSyncType(ctx: SyncTypeContext) = Sync
    override def visitWireType(ctx: WireTypeContext) = Wire
    override val defaultResult = Wire
  }

  object CaseVisitor extends BaseVisitor[AlogicAST] {
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

  object VarRefVisitor extends BaseVisitor[AlogicExpr] {
    override def visitVarRef(ctx: VarRefContext) = visit(ctx.dotted_name)
    override def visitVarRefIndex(ctx: VarRefIndexContext) =
      ArrayLookup(visit(ctx.dotted_name), ExprVisitor(ctx.expr))
    override def visitVarRefSlice(ctx: VarRefSliceContext) =
      BinaryArrayLookup(visit(ctx.dotted_name), ExprVisitor(ctx.expr(0)), ctx.op, ExprVisitor(ctx.expr(1)))

    override def visitLValueCat(ctx: LValueCatContext) = BitCat(visit(ctx.refs))

    override def visitDotted_name(ctx: Dotted_nameContext) = LookupName(ctx, DottedName(ctx.es.toList.map(_.text)))

    // This function handles namespace lookups
    private def LookupName(ctx: ParserRuleContext, dotname: DottedName): DottedName = {
      val s = dotname.names
      val name2 = if (s.length == 1) identifier(s(0)) else dotname
      // Check in namespace and rewrite if necessary
      name2 match {
        case DottedName(ns) => DottedName(NS.lookup(ctx, ns.head) :: ns.tail)
      }
    }
  }

  object ExprVisitor extends BaseVisitor[AlogicExpr] {
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
        case DottedName(names) if (names.length == 1 && names.head == "zxt") => {
          if (a.length != 2)
            Message.error(ctx, s"Zero extend function takes exactly two arguments: number of bits and expression (${a.length} found)")
          Zxt(a(0), a(1))
        }
        case DottedName(names) if (names.length == 1 && names.head == "sxt") => {
          if (a.length != 2)
            Message.error(ctx, s"Sign extend function takes exactly two arguments: number of bits and expression (${a.length} found)")
          Sxt(a(0), a(1))
        }
        case _ => FunCall(n, a)
      }
    }
  }

  object StatementVisitor extends BaseVisitor[AlogicAST] {
    private def is_control_label(cmd: AlogicAST): Boolean = cmd match {
      case ControlCaseLabel(_, _) => true
      case _                      => false
    }

    override def visitBlockStmt(ctx: BlockStmtContext) = {
      NS.addNamespace()
      val ret = visit(ctx.stmts) match {
        case s if (s.length > 0 && is_control_stmt(s.last)) => ControlBlock(s)
        case s if (s.forall(!is_control_stmt(_)))           => CombinatorialBlock(s)
        case s => {
          Message.error(ctx, "A control block must end with a control statement");
          ControlBlock(s)
        }
      }
      NS.removeNamespace()
      ret
    }

    override def visitDeclStmt(ctx: DeclStmtContext) = DeclVisitor(ctx.declaration()) match {
      case s @ VarDeclaration(_, _, _) => DeclarationStmt(s)
      case _ => {
        Message.error(ctx, "Only variable declarations allowed as statements")
        DeclarationStmt(VarDeclaration(State, DottedName(List("Unknown")), None))
      }
    }

    override def visitWhileStmt(ctx: WhileStmtContext) = WhileLoop(ExprVisitor(ctx.expr()), {
      val body = visit(ctx.statement)
      if (!is_control_stmt(body))
        Message.error(ctx, "The body of a while loop must end with a control statement")
      body
    })

    override def visitIfStmt(ctx: IfStmtContext) = {
      val cond = ExprVisitor(ctx.expr())
      val yes = visit(ctx.thenStmt)
      val no = visit(Option(ctx.elseStmt))
      if (is_control_stmt(yes)) no match {
        case None                            => ControlIf(cond, yes, no)
        case Some(s) if (is_control_stmt(s)) => ControlIf(cond, yes, no)
        case _ => {
          Message.error(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements");
          Message.error(ctx, s"1 $no");
          ControlIf(cond, yes, no)
        }
      }
      else no match {
        case None                             => CombinatorialIf(cond, yes, no)
        case Some(s) if (!is_control_stmt(s)) => CombinatorialIf(cond, yes, no)
        case _ => {
          Message.error(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements");
          Message.error(ctx, s"2 $yes");
          Message.error(ctx, s"2 $no");
          CombinatorialIf(cond, yes, no)
        }
      }
    }

    override def visitCaseStmt(ctx: CaseStmtContext) = {
      val test = ExprVisitor(ctx.expr())
      CaseVisitor(ctx.cases) match {
        case stmts if (stmts.forall(is_control_label))     => ControlCaseStmt(test, stmts)
        case stmts if (stmts.forall(!is_control_label(_))) => CombinatorialCaseStmt(test, stmts)
        case stmts => {
          Message.error(ctx, "Either all or none of the case items must be control statements");
          ControlCaseStmt(test, stmts)
        }
      }
    }

    override def visitForStmt(ctx: ForStmtContext) =
      ControlFor(visit(ctx.init), ExprVisitor(ctx.cond), visit(ctx.step), visit(ctx.stmts))

    override def visitDoStmt(ctx: DoStmtContext) =
      ControlDo(ExprVisitor(ctx.expr), visit(ctx.stmts))

    override def visitFenceStmt(ctx: FenceStmtContext) = FenceStmt
    override def visitBreakStmt(ctx: BreakStmtContext) = BreakStmt
    override def visitReturnStmt(ctx: ReturnStmtContext) = ReturnStmt
    override def visitDollarCommentStmt(ctx: DollarCommentStmtContext) = AlogicComment(ctx.LITERAL)
    override def visitGotoStmt(ctx: GotoStmtContext) = GotoStmt(ctx.IDENTIFIER)

    override def visitAssignmentStmt(ctx: AssignmentStmtContext) = visit(ctx.assignment_statement)
    override def visitPrimaryIncStmt(ctx: PrimaryIncStmtContext) = Plusplus(VarRefVisitor(ctx.lvalue))
    override def visitPrimaryDecStmt(ctx: PrimaryDecStmtContext) = Minusminus(VarRefVisitor(ctx.lvalue))
    override def visitAssignStmt(ctx: AssignStmtContext) = Assign(VarRefVisitor(ctx.lvalue), ExprVisitor(ctx.expr()))
    override def visitUpdateStmt(ctx: UpdateStmtContext) = Update(VarRefVisitor(ctx.lvalue), ctx.ASSIGNOP, ExprVisitor(ctx.expr()))

    override def visitExprStmt(ctx: ExprStmtContext) = ExprVisitor(ctx.expr)
  }

  object CommaArgsVisitor extends BaseVisitor[List[AlogicExpr]] {
    override def visitComma_args(ctx: Comma_argsContext) = ExprVisitor(ctx.es)
  }

  object TypeVisitor extends BaseVisitor[AlogicType] {
    override def visitBoolType(ctx: BoolTypeContext) = IntType(false, 1)

    override def visitIntType(ctx: IntTypeContext) = IntType(true, ctx.INTTYPE.text.tail.toInt)

    override def visitUintType(ctx: UintTypeContext) = IntType(false, ctx.UINTTYPE.text.tail.toInt)

    override def visitIdentifierType(ctx: IdentifierTypeContext) = {
      val s = ctx.IDENTIFIER.text
      typedefs.getOrElse(s, {
        Message.error(ctx, s"Unknown type '$s'")
        IntType(false, 1)
      })
    }

    object FieldVisitor extends BaseVisitor[FieldType] {
      override def visitField(ctx: FieldContext) = Field(TypeVisitor(ctx.known_type()), ctx.IDENTIFIER)
    }

    override def visitStructType(ctx: StructTypeContext) = Struct(FieldVisitor(ctx.fields))

    override def visitIntVType(ctx: IntVTypeContext) = IntVType(true, CommaArgsVisitor(ctx.comma_args()))
    override def visitUintVType(ctx: UintVTypeContext) = IntVType(false, CommaArgsVisitor(ctx.comma_args()))
  }

  // Build the abstract syntax tree from a parse tree
  def apply(parseTree: RuleNode): Program = {
    // Add known identifiers that are not already recognised as keywords
    for { id <- List("zxt", "sxt", "go") } NS.insert(id)
    // Capture all found function names into toplevel namespace
    FunVisitor(parseTree)
    // Then build abstract syntax tree and remap identifiers
    Program(ProgVisitor(parseTree))
  }
}
