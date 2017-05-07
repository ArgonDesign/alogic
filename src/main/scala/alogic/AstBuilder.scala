package alogic

import alogic.antlr4._
import alogic.antlr4.VParser._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.ParserRuleContext
import scala.collection._
import scala.collection.mutable.ListBuffer

// The aim of the AstBuilder stage is:
//   Build an abstract syntax tree
//   Deal with as many error conditions as possible (while we can easily report error location)
//   Deal with typedefs
//   Deal with variable scope
//   Deal with #defines
//   Rewrite go/zxt/sxt/read/write function calls
//
// We use different visitors for the different things we wish to extract.
//
// The overall API is to make a AstBuilder class, and then call it to build the ast from a parse tree.
// The object can be called multiple times to allow us to only build a common header file once.
//
// TODO
//   Map reads and writes into the nodes (this allows function calls to count as control statements)
//   Deal with go and zxt and sxt rewrites


class AstBuilder {

  val typedefs = mutable.Map[String,AlogicType]()
  val defines = mutable.Map[String,AlogicAST]() 
  val errors = new ListBuffer[String]()
  val NS = new Namespace(warning)
  
  typedefs("state") = State()
  
  def warning(ctx: ParserRuleContext, msg: String) {
    val tok = ctx.getStart()
    val line = tok.getLine()
    val pos = tok.getCharPositionInLine()
    errors += s"line $line:$pos $msg"
  }
  
  // Add definitions/typedefs from another file
  // Note that identifiers are not copied over
  def add(old: AstBuilder) {
    typedefs ++= old.typedefs
    defines ++= old.defines
  }
  
  // Convert identifier to tree
  def identifier(ident: String): AlogicAST = defines.getOrElse(ident,DottedName(List(ident)))

  object ProgVisitor extends VParserBaseVisitor[List[AlogicAST]] {
    override def visitStart(ctx: StartContext) = {
      ctx.entities.asScala.toList.map(EntityVisitor.visit)
    }
  }
  
  object EntityVisitor extends VParserBaseVisitor[AlogicAST] {
    override def visitTypedef(ctx: TypedefContext) = {
      val s = ctx.IDENTIFIER().getText()
      if (typedefs contains s) {
        warning(ctx,s"Repeated typedef $s")
      } else {
        typedefs(s) = TypeVisitor.visit(ctx.known_type())
      }
      Typedef()
    }
    
    override def visitDefine(ctx: DefineContext) = {
      val s = ctx.IDENTIFIER().getText()
      if (defines contains s) {
        warning(ctx,s"Repeated identifier $s")
      } else {
        defines(s) = Define() // TODO
      }
      Define()
    }
    
    override def visitTask(ctx: TaskContext) = { 
      Task(
        TasktypeVisitor.visit(ctx.tasktype),
        ctx.IDENTIFIER().getText(),
        ctx.decls.asScala.toList.map(DeclVisitor.visit),
        ctx.contents.asScala.toList.map(TaskContentVisitor.visit)
        )
    }
  }
  
  object FunVisitor extends VParserBaseVisitor[Unit] {
    override def visitFunction(ctx: FunctionContext): Unit = NS.insert(ctx,ctx.IDENTIFIER().getText())
  }
  
  object TaskContentVisitor extends VParserBaseVisitor[TaskContent] {
    override def visitFunction(ctx: FunctionContext) = Function(ctx.IDENTIFIER().getText(), ExprVisitor.visit(ctx.statement()))
    override def visitFenceFunction(ctx: FenceFunctionContext) = FenceFunction(ExprVisitor.visit(ctx.statement()))
    override def visitVerilogFunction(ctx: VerilogFunctionContext) = VerilogFunction(VerilogBodyVisitor.visit(ctx.verilogbody()))
  }
  
  object VerilogBodyVisitor extends VParserBaseVisitor[String] {
    override def visitVerilogbody(ctx: VerilogbodyContext) = ctx.tks.asScala.toList.map(visit).mkString
    override def visitVany(ctx: VanyContext) = ctx.VANY().getText()
    override def visitVbody(ctx: VbodyContext) = visit(ctx.verilogbody())
  }
  
  object DeclVisitor extends VParserBaseVisitor[Declaration] {
    override def visitOutDecl(ctx: OutDeclContext) = OutDeclaration(
      Option(ctx.sync_type()).map(SyncTypeVisitor.visit).getOrElse(Wire()),
      TypeVisitor.visit(ctx.known_type()),
      NS.insert(ctx,ctx.IDENTIFIER.getText())
    )
    
    override def visitInDecl(ctx: InDeclContext) = InDeclaration(
      Option(ctx.sync_type()).map(SyncTypeVisitor.visit).getOrElse(Wire()),
      TypeVisitor.visit(ctx.known_type()),
      NS.insert(ctx,ctx.IDENTIFIER.getText())
    )
    
    override def visitConstDecl(ctx: ConstDeclContext) = ConstDeclaration(
      TypeVisitor.visit(ctx.known_type()),
      NS.insert(ctx,ctx.IDENTIFIER.getText()),
      Option(ctx.initializer()).map(ExprVisitor.visit)
    )
    
    override def visitDecl(ctx: DeclContext) = visit(ctx.declaration())

    override def visitVerilogDecl(ctx: VerilogDeclContext) = VerilogDeclaration(
      TypeVisitor.visit(ctx.known_type()),
      InsertExprVisitor.visit(ctx.primary_expr())  
    )
    
    override def visitDeclaration(ctx: DeclarationContext) = VarDeclaration(
      TypeVisitor.visit(ctx.known_type()),
      InsertExprVisitor.visit(ctx.primary_expr()),
      Option(ctx.initializer()).map(ExprVisitor.visit)  
      
      // Insert into NS - tricky because we want to rewrite in visitor - but the identifier is currently unknown
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
  object InsertExprVisitor extends VParserBaseVisitor[AlogicAST] {
    override def visitArrayAccessExpr(ctx: ArrayAccessExprContext) = ArrayLookup(visit(ctx.secondary_expr()),ExprVisitor.visit(ctx.expr()))
    
    override def visitDotted_name(ctx: Dotted_nameContext) = {
      val s = ctx.es.asScala.toList.map(a=>a.getText())
      if (s.length!=1)
        warning(ctx,s"Malformed declaration $s")
      DottedName(List(NS.insert(ctx,s.head)))
    }
  }
  
  object TasktypeVisitor extends VParserBaseVisitor[TaskType] {
    override def visitFsmType(ctx: FsmTypeContext) = Fsm()
    override def visitPipelineType(ctx: PipelineTypeContext) = Pipeline()
    override def visitVerilogType(ctx: VerilogTypeContext) = Verilog()
    override def visitNetworkType(ctx: NetworkTypeContext) = Fsm()
  }
  
  object SyncTypeVisitor extends VParserBaseVisitor[SyncType] {
    override def visitSyncReadyBubbleType(ctx: SyncReadyBubbleTypeContext) = SyncReadyBubble()
    override def visitWireSyncAcceptType(ctx: WireSyncAcceptTypeContext) = WireSyncAccept()
    override def visitSyncReadyType(ctx: SyncReadyTypeContext) = SyncReady()
    override def visitWireSyncType(ctx: WireSyncTypeContext) = WireSync()
    override def visitSyncAcceptType(ctx: SyncAcceptTypeContext) = SyncAccept()
    override def visitSyncType(ctx: SyncTypeContext) = Sync()
    override def visitWireType(ctx: WireTypeContext) = Wire()
  }
  
  object CaseVisitor extends VParserBaseVisitor[CaseLabel] {
    override def visitDefaultCase(ctx: DefaultCaseContext) = {
      val s = ExprVisitor.visit(ctx.statement())
      if (is_control_stmt(s))
        ControlCaseLabel(List(),s)
      else
        CombinatorialCaseLabel(List(),s)
    }
    
    override def visitNormalCase(ctx: NormalCaseContext) = {
      val s = ExprVisitor.visit(ctx.statement())
      val args = CommaArgsVisitor.visit(ctx.comma_args())
      if (is_control_stmt(s))
        ControlCaseLabel(args,s)
      else
        CombinatorialCaseLabel(args,s)
    }
  }
    
  // Statement visitors
  def is_control_stmt(cmd: AlogicAST) : Boolean = cmd match {
      case FenceStmt() => true
      case BreakStmt() => true
      case ReturnStmt() => true
      case GotoStmt(target) => true
      case ControlBlock(s) => true
      case ControlIf(cond,body,elsebody) => true
      case WhileLoop(cond,body) => true
      case ControlFor(_,_,_,_) => true
      case ControlDo(_,_) => true
      case ControlCaseStmt(_,_) => true
      case FunCall(_,_) => true
      case _ => false
    }
  
  object ExprVisitor extends VParserBaseVisitor[AlogicAST] {
    override def visitTernaryExpr(ctx: TernaryExprContext) = TernaryOp(visit(ctx.binary_expr()), visit(ctx.expr(0)), visit(ctx.expr(1)))
    override def visitBinaryExpr(ctx: BinaryExprContext) = BinaryOp(visit(ctx.unary_expr()), ctx.binary_op().getText(), visit(ctx.expr()))
    override def visitUnaryExpr(ctx: UnaryExprContext) = UnaryOp(ctx.unary_op().getText(), visit(ctx.primary_expr()))
    override def visitArrayAccessExpr(ctx: ArrayAccessExprContext) = ArrayLookup(visit(ctx.secondary_expr()),visit(ctx.expr()))
    override def visitArrayAccess2Expr(ctx: ArrayAccess2ExprContext) = BinaryArrayLookup(
      visit(ctx.secondary_expr()),visit(ctx.expr(0)),ctx.arrayop().getText(),visit(ctx.expr(1))
    )
    override def visitTrueExpr(ctx: TrueExprContext) = Num("1'b1")
    override def visitFalseExpr(ctx: FalseExprContext) = Num("1'b0")
    override def visitBracketExpr(ctx: BracketExprContext) = Bracket(visit(ctx.expr()))
    override def visitTicknumExpr(ctx: TicknumExprContext) = Num(ctx.TICKNUM().getText())
    override def visitConstantTickNumExpr(ctx: ConstantTickNumExprContext) = Num(ctx.CONSTANT().getText()+ctx.TICKNUM().getText())
    override def visitIdentifierTickNumExpr(ctx: IdentifierTickNumExprContext) = {
      val id = identifier(ctx.IDENTIFIER().getText())
      val tick = ctx.TICKNUM().getText()
      id match {
        case Num(s) => Num(s+tick)
        case _ => { warning(ctx, "Cannot build a number from $id$tick"); Num("Unknown") }
      } 
    }
    override def visitConstantExpr(ctx: ConstantExprContext) = Num(ctx.CONSTANT().getText())
    override def visitLiteralExpr(ctx: LiteralExprContext) = Literal(ctx.LITERAL().getText())
    override def visitBitRepExpr(ctx: BitRepExprContext) = BitRep(visit(ctx.expr(0)),visit(ctx.expr(1)))
    override def visitBitCatExpr(ctx: BitCatExprContext) = BitCat(CommaArgsVisitor.visit(ctx.comma_args()))
    override def visitFunCallExpr(ctx: FunCallExprContext) = FunCall(visit(ctx.dotted_name()),CommaArgsVisitor.visit(ctx.comma_args()))
    override def visitDollarExpr(ctx: DollarExprContext) = DollarCall(ctx.DOLLAR().getText(),CommaArgsVisitor.visit(ctx.comma_args()))
    override def visitDotted_name(ctx: Dotted_nameContext) = LookupName(ctx, DottedName(ctx.es.asScala.toList.map(a=>a.getText())))
    
    // This function handles #defines and namespace lookups
    // Convert using #defines where necessary
    def LookupName(ctx: ParserRuleContext, dotname: DottedName) : AlogicAST = {
      val s = dotname.names
      // Check for a #define conversion
      val name2 = if (s.length == 1) identifier(s(0)) else dotname
      // Check in namespace and rewrite if necessary
      name2 match {
        case DottedName(ns) => DottedName(NS.lookup(ctx,ns.head) :: ns.tail)
        case x => x
      }
    }
      
    def is_control_label(cmd: CaseLabel) : Boolean = cmd match {
        case ControlCaseLabel(_,_) => true
        case _ => false
    }

    override def visitBlockStmt(ctx: BlockStmtContext) = {
      NS.addNamespace()
      val ret = ctx.stmts.asScala.toList.map(visit) match {
        case s if (s.length>0 && is_control_stmt(s.last)) => ControlBlock(s) 
        case s if (s.forall(x => !is_control_stmt(x))) => CombinatorialBlock(s)
        case s => { warning(ctx, "A control block must end with a control statement"); ControlBlock(s) }
      }
      NS.removeNamespace()
      ret
    }
    
    override def visitDeclStmt(ctx: DeclStmtContext) = DeclVisitor.visit(ctx.declaration()) match {
      case s @ VarDeclaration(_,_,_) => DeclarationStmt(s)
      case _ => { warning(ctx,"Only variable declarations allowed as statements"); DeclarationStmt( VarDeclaration(State(),DottedName(List("Unknown")),None) ) }
    }  
    
    override def visitWhileStmt(ctx: WhileStmtContext) = WhileLoop(visit(ctx.expr()), {
        val body = visit(ctx.statement)
        if (!is_control_stmt(body))
          warning(ctx,"The body of a while loop must end with a control statement")
        body
      }
    )
    
    override def visitIfStmt(ctx: IfStmtContext) = {
      val cond = visit(ctx.expr())
      val yes = visit(ctx.statement())
      val no = Option(ctx.else_statement()).map(visit)
      if (is_control_stmt(yes)) no match {
        case None => ControlIf(cond,yes,no)
        case Some(s) if (is_control_stmt(s)) => ControlIf(cond,yes,no)
        case _ => { warning(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements"); ControlIf(cond,yes,no) }
      } else no match {
        case None => CombinatorialIf(cond,yes,no)
        case Some(s) if (!is_control_stmt(s)) => CombinatorialIf(cond,yes,no)
        case _ => { warning(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements"); CombinatorialIf(cond,yes,no) }
      }
    }
    
    override def visitCaseStmt(ctx: CaseStmtContext) = {
      val test = visit(ctx.expr())
      ctx.cases.asScala.toList.map(CaseVisitor.visit) match {
        case stmts if (stmts.forall(is_control_label))  => ControlCaseStmt(test,stmts)   
        case stmts if (stmts.forall(x=> !is_control_label(x))) => CombinatorialCaseStmt(test,stmts)
        case stmts => { warning(ctx, "Either all or none of the case items must be control statements"); ControlCaseStmt(test,stmts) }
      }
    }
    
    override def visitForStmt(ctx: ForStmtContext) = ControlFor(
      visit(ctx.single_statement(0)), 
      visit(ctx.expr()),
      visit(ctx.single_statement(1)),
      ctx.stmts.asScala.toList.map(visit)
    )
    
    override def visitDoStmt(ctx: DoStmtContext) = ControlDo(
      visit(ctx.expr()),
      ctx.stmts.asScala.toList.map(visit)
    )
    
    override def visitSingleStmt(ctx: SingleStmtContext) = visit(ctx.single_statement())
    override def visitPrimaryIncStmt(ctx: PrimaryIncStmtContext) = Plusplus(visit(ctx.primary_expr()))
    override def visitPrimaryDecStmt(ctx: PrimaryDecStmtContext) = Minusminus(visit(ctx.primary_expr()))
    override def visitAssignStmt(ctx: AssignStmtContext) = Assign(visit(ctx.primary_expr()), ctx.assign_op.getText(), visit(ctx.expr()))
    override def visitFenceStmt(ctx: FenceStmtContext) = FenceStmt()
    override def visitBreakStmt(ctx: BreakStmtContext) = BreakStmt()
    override def visitReturnStmt(ctx: ReturnStmtContext) = ReturnStmt()
    override def visitDollarCommentStmt(ctx: DollarCommentStmtContext) = AlogicComment(ctx.LITERAL().getText())
    override def visitGotoStmt(ctx: GotoStmtContext) = GotoStmt(ctx.IDENTIFIER().getText()) 
  }
  
  object CommaArgsVisitor extends VParserBaseVisitor[List[AlogicAST]] {
    override def visitComma_args(ctx: Comma_argsContext) = ctx.es.asScala.toList.map(ExprVisitor.visit)
  }
  
  object FieldVisitor extends VParserBaseVisitor[FieldType] {
    override def visitField(ctx: FieldContext) = Field(TypeVisitor.visit(ctx.known_type()),ctx.IDENTIFIER.getText())
  }
  
  object TypeVisitor extends VParserBaseVisitor[AlogicType] {
    override def visitBoolType(ctx: BoolTypeContext) = IntType(false,1)
    
    override def visitIntType(ctx: IntTypeContext) = {
      val s = ctx.INTTYPE().getText()
      val n = s.substring(1,s.length)
      IntType(true,n.toInt)
    }
    
    override def visitUintType(ctx: UintTypeContext) = {
      val s = ctx.UINTTYPE().getText()
      val n = s.substring(1,s.length)
      IntType(false,n.toInt)
    }
    
    override def visitIdentifierType(ctx: IdentifierTypeContext) = {
      val s = ctx.IDENTIFIER().getText()
      typedefs.getOrElse( s, {
        warning(ctx,s"Unknown type $s")
        IntType(false,1)
      } )
    }
    
    override def visitStructType(ctx: StructTypeContext) = {
      Struct(ctx.fields.asScala.toList.map(FieldVisitor.visit))
    }
    
    override def visitIntVType(ctx: IntVTypeContext) = IntVType(true,ExprVisitor.visit(ctx.expr()))
    override def visitUintVType(ctx: UintVTypeContext) = IntVType(false,ExprVisitor.visit(ctx.expr()))
  }
  
  // Return if this node is a task node
  def is_task(ast:AlogicAST) : Boolean = ast match {case Task(_,_,_,_) => true; case _ => false}
  
  // Build the abstract syntax tree from a parse tree
  def apply(parseTree : ParseTree) : Program = {
    // First capture all function names into toplevel namespace
    FunVisitor.visit(parseTree)
    // Then build abstract syntax tree and remap identifiers
    val p = Program(ProgVisitor.visit(parseTree).filter(is_task))
    errors.foreach(println)
    p
  }
}

