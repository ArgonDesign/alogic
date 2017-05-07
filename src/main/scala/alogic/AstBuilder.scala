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
//
// We use different visitors for the different things we wish to extract.
//
// The overall API is to make a AstBuilder class, and then call it to build the ast from a parse tree.
// The object can be called multiple times to allow us to only build a common header file once.

class AstBuilder {

  val typedefs = mutable.Map[String,AlogicType]()
  val defines = mutable.Map[String,AlogicAST]()
  
  val errors = new ListBuffer[String]()
  
  def warning(ctx: ParserRuleContext, msg: String) {
    val tok = ctx.getStart()
    val line = tok.getLine()
    val pos = tok.getCharPositionInLine()
    errors += s"line $line:$pos $msg"
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
        ctx.decls.asScala.toList.map(TaskDeclVisitor.visit),
        Nil //TODO ctx.contents.asScala.toList.map(TaskContentVisitor.visit)
        )
    }
  }
  
  object TaskDeclVisitor extends VParserBaseVisitor[Declaration] {
    override def visitOutDecl(ctx: OutDeclContext) = OutDeclaration(
      Option(ctx.sync_type()).map(SyncTypeVisitor.visit).getOrElse(Wire()),
      TypeVisitor.visit(ctx.known_type()),
      ctx.IDENTIFIER.getText()
    )
    
    override def visitInDecl(ctx: InDeclContext) = InDeclaration(
      Option(ctx.sync_type()).map(SyncTypeVisitor.visit).getOrElse(Wire()),
      TypeVisitor.visit(ctx.known_type()),
      ctx.IDENTIFIER.getText()
    )
    
    override def visitConstDecl(ctx: ConstDeclContext) = ConstDeclaration(
      TypeVisitor.visit(ctx.known_type()),
      Name(ctx.IDENTIFIER.getText()),
      Option(ctx.initializer()).map(ExprVisitor.visit)
    )
    // TODO verilog
    // TODO decl
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
    override def visitDotted_name(ctx: Dotted_nameContext) = DottedName(ctx.es.asScala.toList.map(a=>a.getText()))
    
    
  }
  
  object CommaArgsVisitor extends VParserBaseVisitor[List[AlogicAST]] {
    override def visitComma_args(ctx: Comma_argsContext) = ctx.es.asScala.toList.map(ExprVisitor.visit)
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
    
    // TODO IntVType and UintVType and Struct
    
  }
  
  // Return if this node is a task node
  def is_task(ast:AlogicAST) : Boolean = ast match {case Task(_,_,_,_) => true; case _ => false}
  
  // Build the abstract syntax tree from a parse tree
  def apply(parseTree : ParseTree) : Program = {
    val p = Program(ProgVisitor.visit(parseTree).filter(is_task))
    errors.foreach(println)
    p
  }
}

