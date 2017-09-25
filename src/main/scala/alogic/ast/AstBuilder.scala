////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import scala.collection.immutable.ListMap
import org.antlr.v4.runtime.ParserRuleContext
import alogic.Antlr4Conversions._
import alogic.Message
import alogic.VScalarVisitor
import alogic.antlr.VParser._

// The aim of the AstBuilder stage is:
//   Build an abstract syntax tree
//   Deal with as many error conditions as possible (while we can easily report error location)
//   Deal with typedefs
//   Deal with variable scope
//
// We use different visitors for the different things we wish to extract.

////////////////////////////////////////////////////////////////////////////////
// Visitors and data structures required when parsing all kinds of source files
////////////////////////////////////////////////////////////////////////////////
class CommonContext(root: ParserRuleContext, initialTypedefs: Map[String, Type]) {
  // Collect type definitions and entityContext
  val (typedefs, entityCtx) = root match {
    case ctx: StartContext   => ExtractTypedefs(ctx, initialTypedefs)
    case ctx: TaskFSMContext => (initialTypedefs, ctx)
  }

  // Build scopes and allocate static variable names
  private[this] val symtab = new Symtab(root, typedefs)

  val exprVisitor = new ExprVisitor(Some(symtab), typedefs)

  val knownTypeVisitor = new KnownTypeVisitor(Some(symtab), typedefs)

  // Construct tree for variable reference, looking up the variable name in the current scope
  object VarRefVisitor extends VScalarVisitor[VarRef] {
    // Construct tree for dotted name, looking up the variable name in the current scope
    object LookUpName extends VScalarVisitor[DottedName] {
      override def visitDotted_name(ctx: Dotted_nameContext) = {
        val (head :: tail) = ctx.es.toList.map(_.text)
        DottedName(symtab(ctx, head).left.get.id :: tail)
      }
    }
    override def visitVarRef(ctx: VarRefContext) = LookUpName(ctx.dotted_name)
    override def visitVarRefIndex(ctx: VarRefIndexContext) = ArrayLookup(LookUpName(ctx.dotted_name), exprVisitor(ctx.es))
  }

  object DeclVisitor extends VScalarVisitor[Declaration] {
    object VarRefId extends VScalarVisitor[String] {
      override def visitVarRef(ctx: VarRefContext) = ctx.dotted_name.text
      override def visitVarRefIndex(ctx: VarRefIndexContext) = ctx.dotted_name.text
    }

    override def visitTaskDeclOut(ctx: TaskDeclOutContext) = symtab(ctx, ctx.IDENTIFIER).left.get
    override def visitTaskDeclIn(ctx: TaskDeclInContext) = symtab(ctx, ctx.IDENTIFIER).left.get
    override def visitTaskDeclConst(ctx: TaskDeclConstContext) = symtab(ctx, ctx.IDENTIFIER).left.get
    override def visitTaskDeclPipeline(ctx: TaskDeclPipelineContext) = symtab(ctx, ctx.IDENTIFIER).left.get
    override def visitTaskDeclParam(ctx: TaskDeclParamContext) = symtab(ctx, ctx.IDENTIFIER).left.get
    override def visitTaskDeclVerilog(ctx: TaskDeclVerilogContext) = symtab(ctx, VarRefId(ctx.var_ref)).left.get
    override def visitTaskDecl(ctx: TaskDeclContext) = visit(ctx.decl)
    override def visitDeclNoInit(ctx: DeclNoInitContext) = symtab(ctx, VarRefId(ctx.var_ref)).left.get
    override def visitDeclInit(ctx: DeclInitContext) = symtab(ctx, VarRefId(ctx.var_ref)).left.get
  }

  object VerilogFunctionVisitor extends VScalarVisitor[VerilogFunction] {
    override def visitVerilogFunction(ctx: VerilogFunctionContext) = VerilogFunction(ctx.VERILOGBODY.text.drop(1).dropRight(1))
  }
}

////////////////////////////////////////////////////////////////////////////////
// Builder to handle 'fsm' task definitions
////////////////////////////////////////////////////////////////////////////////
class FsmTaskBuilder(cc: CommonContext) {
  import cc._

  def apply(tree: TaskFSMContext): FsmTask = {

    object LValueVisitor extends VScalarVisitor[Expr] {
      override def visitLValue(ctx: LValueContext) = VarRefVisitor(ctx.var_ref)
      override def visitLValueSlice(ctx: LValueSliceContext) =
        Slice(VarRefVisitor(ctx.var_ref), exprVisitor(ctx.expr(0)), ctx.op, exprVisitor(ctx.expr(1)))
      override def visitLValueCat(ctx: LValueCatContext) =
        BitCat(visit(ctx.refs))
    }

    object StatementVisitor extends VScalarVisitor[Stmt] {
      override def visitBlockStmt(ctx: BlockStmtContext) = {
        val stmts = visit(ctx.stmts)
        val ctrlStmts = stmts collect { case s: CtrlStmt => s }
        val combStmts = stmts collect { case s: CombStmt => s }

        (ctrlStmts, combStmts) match { // TODO: Not sure this is the best way to write this
          case (Nil, comb) => CombinatorialBlock(comb)
          case (ctrl, comb) => {
            stmts.last match {
              case s: CtrlStmt => ControlBlock(stmts)
              case s: CombStmt => Message.error(ctx, "A control block must end with a control statement"); ErrorStmt
            }
          }
        }
      }

      override def visitDeclStmt(ctx: DeclStmtContext) = DeclVisitor(ctx.decl) match {
        case s: VarDeclaration => DeclarationStmt(s)
        case Declaration(decltype, id) => {
          Message.error(ctx, "Only variable declarations allowed inside functions"); ErrorStmt
        }
      }

      override def visitIfStmt(ctx: IfStmtContext) = {
        val cond = exprVisitor(ctx.expr())
        val thenStmt = visit(ctx.thenStmt)
        val elseStmt = visit(Option(ctx.elseStmt))

        (thenStmt, elseStmt) match {
          case (t: CtrlStmt, None)              => ControlIf(cond, t, None)
          case (t: CtrlStmt, Some(e: CtrlStmt)) => ControlIf(cond, t, Some(e))
          case (t: CombStmt, None)              => CombinatorialIf(cond, t, None)
          case (t: CombStmt, Some(e: CombStmt)) => CombinatorialIf(cond, t, Some(e))
          case _ => {
            Message.error(ctx, "Both branches of an if must be control statements, or both must be combinatorial statements");
            ErrorStmt
          }
        }
      }

      override def visitCaseStmt(ctx: CaseStmtContext) = {

        object DefaultVisitor extends VScalarVisitor[Option[Stmt]] {
          override val defaultResult = None
          override def visitDefaultCase(ctx: DefaultCaseContext) = Some(StatementVisitor(ctx.statement()))
        }

        object CaseVisitor extends VScalarVisitor[Option[Node]] {
          override val defaultResult = None
          override def visitNormalCase(ctx: NormalCaseContext) = {
            val args = exprVisitor(ctx.commaexpr)
            StatementVisitor(ctx.statement()) match {
              case s: CtrlStmt => Some(ControlCaseLabel(args, s))
              case s: CombStmt => Some(CombinatorialCaseLabel(args, s))
            }
          }
        }

        val test = exprVisitor(ctx.expr())

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
            ErrorStmt
          }
        }
      }

      override def visitLoopStmt(ctx: LoopStmtContext) = {
        val body = visit(ctx.stmts)

        body.last match {
          case _: CombStmt => {
            Message.error(ctx, "The body of a 'loop' must end with a control statement"); ErrorStmt
          }
          case _ => ControlLoop(ControlBlock(body))
        }
      }

      def loopWarnings(cond: Expr, body: List[Stmt], condCtx: ParserRuleContext, stmtLastCtx: Option[ParserRuleContext]) = {
        if (cond.isConst) {
          if (cond.eval == 0) {
            Message.warning(condCtx, "Condition of loop is always false")
          } else {
            Message.warning(condCtx, "Condition of loop is always true. Use 'loop' instead.")
          }
        }

        stmtLastCtx match {
          case Some(ctx) => body.last match {
            case FenceStmt => Message.warning(ctx, "Redundant 'fence' at end of loop body")
            case _         =>
          }
          case None =>
        }
      }

      override def visitWhileStmt(ctx: WhileStmtContext) = {
        val cond = exprVisitor(ctx.expr)
        val body = visit(ctx.stmts)

        loopWarnings(cond, body, ctx, ctx.stmts.lastOption)

        ControlWhile(cond, body)
      }

      override def visitDoStmt(ctx: DoStmtContext) = {
        val cond = exprVisitor(ctx.expr)
        val body = visit(ctx.stmts)

        loopWarnings(cond, body, ctx, ctx.stmts.lastOption)

        ControlDo(cond, body)
      }

      object ForInitVisitor extends VScalarVisitor[(Option[VarDeclaration], CombStmt)] {
        override def visitForInitNoDecl(ctx: ForInitNoDeclContext) = {
          val stmt = StatementVisitor(ctx.assignment_statement) match {
            case s: CombStmt => s
            case _: CtrlStmt => unreachable
          }
          (None, stmt)
        }
        override def visitDeclInit(ctx: DeclInitContext) = {
          val varDecl = DeclVisitor(ctx).asInstanceOf[VarDeclaration]
          val initExpr = Assign(VarRefVisitor(ctx.var_ref), exprVisitor(ctx.expr))
          (Some(varDecl), initExpr)
        }
      }

      override def visitForStmt(ctx: ForStmtContext) = {
        val (optDecl, initStmt) = ForInitVisitor(ctx.init)
        val cond = exprVisitor(ctx.cond)
        val stepStmt = visit(ctx.step) match {
          case s: CombStmt => s
          case _: CtrlStmt => unreachable
        }
        val body = visit(ctx.stmts)

        loopWarnings(cond, body, ctx, ctx.stmts.lastOption)

        val forAST = ControlFor(initStmt, cond, stepStmt, body)
        optDecl match {
          case None       => forAST
          case Some(decl) => ControlBlock(DeclarationStmt(decl) :: forAST :: Nil)
        }
      }

      override def visitFenceStmt(ctx: FenceStmtContext) = FenceStmt
      override def visitBreakStmt(ctx: BreakStmtContext) = BreakStmt
      override def visitReturnStmt(ctx: ReturnStmtContext) = ReturnStmt
      override def visitDollarCommentStmt(ctx: DollarCommentStmtContext) = AlogicComment(ctx.LITERAL)
      override def visitGotoStmt(ctx: GotoStmtContext) = GotoStmt(ctx.IDENTIFIER)

      override def visitAssignmentStmt(ctx: AssignmentStmtContext) = visit(ctx.assignment_statement)

      override def visitAssignInc(ctx: AssignIncContext) = Plusplus(LValueVisitor(ctx.leftvalue))
      override def visitAssignDec(ctx: AssignDecContext) = Minusminus(LValueVisitor(ctx.leftvalue))
      override def visitAssign(ctx: AssignContext) = Assign(LValueVisitor(ctx.leftvalue), exprVisitor(ctx.expr()))
      override def visitAssignUpdate(ctx: AssignUpdateContext) = Update(LValueVisitor(ctx.leftvalue), ctx.ASSIGNOP, exprVisitor(ctx.expr()))

      override def visitExprStmt(ctx: ExprStmtContext) = exprVisitor(ctx.expr) match {
        case CallExpr(DottedName(target :: Nil), Nil) => CallStmt(target)
        case CallExpr(_, _)                           => unreachable
        case expr => {
          if (expr.hasSideEffect) {
            ExprStmt(expr)
          } else {
            Message.error(ctx, "A pure expression in statement position does nothing"); ErrorStmt
          }
        }
      }
    }

    object FsmTaskVisitor extends VScalarVisitor[FsmTask] {
      object FsmContentVisitor extends VScalarVisitor[Node] {
        override def visitFunction(ctx: FunctionContext) = {
          val name = ctx.IDENTIFIER.text
          val stmts = StatementVisitor(ctx.stmts)
          val body = stmts.last match {
            case _: CtrlStmt => ControlBlock(stmts)
            case _: CombStmt => {
              Message.error(ctx, "A function body must end with a control statement"); ErrorStmt
            }
          }
          Function(name, body)
        }
        override def visitFenceFunction(ctx: FenceFunctionContext) = {
          val body = StatementVisitor(ctx.stmts) collect {
            case stmt: CombStmt => stmt
            case stmt: CtrlStmt => Message.error(ctx, "Body of 'fence' function must not contain control statements"); ErrorStmt
          }
          FenceFunction(CombinatorialBlock(body))
        }
        override def visitVerilogFunction(ctx: VerilogFunctionContext) = VerilogFunctionVisitor(ctx)
      }

      override def visitTaskFSM(ctx: TaskFSMContext) = {
        val name = ctx.IDENTIFIER.text
        val decls = DeclVisitor(ctx.decls)
        val contents = FsmContentVisitor(ctx.contents)
        val fns = contents collect { case x: Function => x }
        val fencefns = contents collect { case x: FenceFunction => x }
        val vfns = contents collect { case x: VerilogFunction => x }
        val hasNew = Option(ctx.autoinst).isDefined;
        if (fencefns.length > 1) {
          Message.error(ctx, s"fsm '$name' has more than 1 fence function defined")
        }

        FsmTask(name, decls, fns, fencefns.headOption, vfns, hasNew)
      }
    }

    FsmTaskVisitor(tree)
  }
}

////////////////////////////////////////////////////////////////////////////////
// Builder to handle 'verilog' task definitions
////////////////////////////////////////////////////////////////////////////////
class VerilogTaskBuilder(cc: CommonContext) {
  import cc._

  def apply(tree: TaskVerilogContext): VerilogTask = {

    object VerilogTaskVisitor extends VScalarVisitor[VerilogTask] {
      override def visitTaskVerilog(ctx: TaskVerilogContext) = {
        val vfns = ctx.contents.toList collect {
          case c: VerilogFunctionContext => VerilogFunctionVisitor(c)
        }
        VerilogTask(ctx.IDENTIFIER.text, DeclVisitor(ctx.decls), vfns)
      }
    }

    VerilogTaskVisitor(tree)
  }
}

////////////////////////////////////////////////////////////////////////////////
// Builder to handle 'network' definitions
////////////////////////////////////////////////////////////////////////////////
class NetworkTaskBuilder(cc: CommonContext) {
  import cc._

  lazy val fsmTaskBuilder = new FsmTaskBuilder(cc)

  def apply(tree: NetworkContext): NetworkTask = {

    object DottedNameVisitor extends VScalarVisitor[DottedName] {
      override def visitDotted_name(ctx: Dotted_nameContext) = DottedName(ctx.es.toList map (_.text))
    }

    object NetworkVisitor extends VScalarVisitor[NetworkTask] {
      object NetworkContentVisitor extends VScalarVisitor[Node] {
        override def visitTaskFSM(ctx: TaskFSMContext) = fsmTaskBuilder(ctx) //, typedefs)
        override def visitTaskVerilog(ctx: TaskVerilogContext) = ???
        override def visitConnect(ctx: ConnectContext) = {
          val lhs = DottedNameVisitor(ctx.lhs)
          val rhs = DottedNameVisitor(ctx.rhs)
          Connect(lhs, rhs)
        }
        override def visitInstantiate(ctx: InstantiateContext) = {
          val id = ctx.IDENTIFIER(0).text
          val module = ctx.IDENTIFIER(1).text
          val pas = ctx.param_args.param_assign.toList map { pa =>
            pa.IDENTIFIER.text -> exprVisitor(pa.expr)
          }
          Instantiate(id, module, ListMap(pas: _*))
        }
        override def visitVerilogFunction(ctx: VerilogFunctionContext) = VerilogFunctionVisitor(ctx)
      }

      override def visitNetwork(ctx: NetworkContext) = {
        val name = ctx.IDENTIFIER.text
        val decls = DeclVisitor(ctx.decls)
        val contents = NetworkContentVisitor(ctx.contents)
        val inst = contents collect {
          case x: Instantiate                                 => x
          case FsmTask(name, decls, fns, fencefn, vfns, true) => Instantiate(name, name, ListMap.empty)
        }
        val conn = contents collect { case x: Connect => x }
        val vfns = contents collect { case x: VerilogFunction => x }
        val fsms = contents collect { case x: FsmTask => x }

        NetworkTask(name, decls, inst, conn, vfns, fsms)
      }
    }
    NetworkVisitor(tree)
  }
}

object AstBuilder {
  def apply(root: ParserRuleContext, initialTypedefs: Map[String, Type] = Map[String, Type]()): Task = {

    val cc = new CommonContext(root, initialTypedefs)

    lazy val fsmTaskBuilder = new FsmTaskBuilder(cc)
    lazy val verilogTaskBuilder = new VerilogTaskBuilder(cc)
    lazy val networkTaskBuilder = new NetworkTaskBuilder(cc)

    object RootVisitor extends VScalarVisitor[Task] {
      override def visitTaskFSM(ctx: TaskFSMContext) = fsmTaskBuilder(ctx)
      override def visitTaskVerilog(ctx: TaskVerilogContext) = verilogTaskBuilder(ctx)
      override def visitNetwork(ctx: NetworkContext) = networkTaskBuilder(ctx)
    }

    RootVisitor(cc.entityCtx)
  }
}
