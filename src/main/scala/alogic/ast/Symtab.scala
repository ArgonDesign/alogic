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
import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.RuleContext
import alogic.Antlr4Conversions._
import alogic.antlr.VParser._
import alogic.Loc
import alogic.Message
import alogic.VScalarVisitor

// This class constructs the lexical scopes of the provided parse tree.
// We use standard mutable.Map instances with the 'withDefault' extension
// to map back to outer scopes. When the instance is created, it walks
// the parse tree and constructs the scopes from the appropriate declarations
// in the program. The resulting object can then be used to look up names,
// but is otherwise immutable from the outside.
//
// Variable instances with a multiplicity greater than 1 are renamed as
// <name> -> <name>_L<lineno> where <lineno> is the line number where the
// variable is declared.
class Symtab(root: ParserRuleContext, typedefs: scala.collection.Map[String, Type]) { self =>

  // A named pair of Decl and the location of the Decl. For data declarations,
  // we use Left[Decl], for function declarations, we use Right[Decl]
  private[this] case class Item(decl: Either[Decl, String], ctx: ParserRuleContext)

  // This is a map to Option[_] as Map.get does not respect withDefault or withDefaultValue
  private[this]type NameMap = mutable.Map[String, Option[Item]]

  // Map from parse tree node to containing scope
  private[this] val scopes = mutable.Map[RuleContext, NameMap]()

  // Keep track of multiplicity of statically allocated names
  private[this] val multiplicity = mutable.Map[String, Int]().withDefaultValue(0)

  // Find the containing Scope of this parse tree node
  // Note that this lazily builds the full map as needed
  private[this] def find(ctx: RuleContext): NameMap = scopes.getOrElseUpdate(ctx, find(ctx.parent))

  // Create new scope at node ctx
  private[this] def create(ctx: RuleContext) = {
    if (ctx != root) {
      if (scopes contains ctx) {
        // If the mapping already exists, then it must be a transitive scope of
        // the parent created by the getOrElseUpdate of the find method above
        assert(scopes(ctx) == scopes(ctx.parent))
      }
      scopes(ctx) = mutable.Map[String, Option[Item]]().withDefault(find(ctx.parent))
    }
  }

  // Insert name into scope of node ctx
  private[this] def insert(ctx: ParserRuleContext, decl: Either[Decl, String]): Unit = {
    val name = decl match {
      case Left(d)   => d.id
      case Right(id) => id
    }
    val scope = find(ctx)
    if (scope contains name) {
      val Some(Item(_, pctx)) = scope(name)
      Message.error(ctx, s"Multiple declarations of name '$name' ...")
      Message.error(ctx, s"... previous Decl at: ${pctx.loc}")
    } else {
      scope(name) match {
        case Some(Item(_, pctx)) => {
          Message.warning(ctx, s"Decl of '$name' hides previous Decl of same name at ...")
          Message.warning(ctx, s"... ${pctx.loc}")
        }
        case None => ()
      }
    }
    scope(name) = Some(Item(decl, ctx)) // Insert to 'scope'
    multiplicity(name) += {
      decl match {
        case Left(_: DeclVar) => 1
        case Left(_: DeclArr) => 1
        case _                => 0
      }
    }
  }

  private[this] object DeclExtractor extends VScalarVisitor[Decl] {
    object PortTypeVisitor extends VScalarVisitor[Type] {
      override def visitPortTypeKnown(ctx: PortTypeKnownContext) = {
        val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
        knownTypeVisitor(ctx)
      }
      override def visitPortTypeVoid(ctx: PortTypeVoidContext) = VoidType
    }

    object StorageTypeVisitor extends VScalarVisitor[StorageType] {
      override def visitStorageTypeWire(ctx: StorageTypeWireContext) = StorageTypeWire
      override def visitStorageTypeBubble(ctx: StorageTypeBubbleContext) = StorageTypeBubble
      override val defaultResult = StorageTypeReg
    }

    object FlowControlTypeVisitor extends VScalarVisitor[FlowControlType] {
      override def visitFlowControlTypeSync(ctx: FlowControlTypeSyncContext) = FlowControlTypeValid
      override def visitFlowControlTypeSyncReady(ctx: FlowControlTypeSyncReadyContext) = FlowControlTypeReady
      override def visitFlowControlTypeSyncAccept(ctx: FlowControlTypeSyncAcceptContext) = FlowControlTypeAccept
      override val defaultResult = FlowControlTypeNone
    }

    override def visitTaskDeclOut(ctx: TaskDeclOutContext) = {
      val fctype = FlowControlTypeVisitor(ctx.flow_control_type)
      val kind = PortTypeVisitor(ctx.port_type)
      val id = ctx.IDENTIFIER.text
      val stype = StorageTypeVisitor(ctx.storage_type)
      DeclOut(kind, id, fctype, stype)
    }
    override def visitTaskDeclIn(ctx: TaskDeclInContext) = {
      val fctype = FlowControlTypeVisitor(ctx.flow_control_type)
      val kind = PortTypeVisitor(ctx.port_type)
      val id = ctx.IDENTIFIER.text
      DeclIn(kind, id, fctype)
    }
    override def visitTaskDeclConst(ctx: TaskDeclConstContext) = {
      val id = ctx.IDENTIFIER.text
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type) match {
        case x: ScalarType => x
        case x             => Message.error(ctx, s"Constant '${id}' must be declared with scalar type"); IntType(false, 1);
      }
      DeclConst(kind, id, ErrorExpr)
    }
    override def visitTaskDeclParam(ctx: TaskDeclParamContext) = {
      val id = ctx.IDENTIFIER.text
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type) match {
        case x: ScalarType => x
        case x             => Message.error(ctx, s"Parameter '${id}' must be declared with scalar type"); IntType(false, 1);
      }
      DeclParam(kind, id, ErrorExpr)
    }
    override def visitTaskDeclPipeline(ctx: TaskDeclPipelineContext) = {
      val id = ctx.IDENTIFIER.text
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type);
      DeclPippeVar(kind, id)
    }
    override def visitTaskDeclVerilog(ctx: TaskDeclVerilogContext) = visit(ctx.decl) match {
      case DeclVar(_, id, Some(_)) => Message.fatal(ctx, s"Decl of Verilog variable '${id}' cannot use initializer")
      case DeclVar(kind, id, None) => DeclVerilogVar(kind, id)
      case DeclArr(kind, id, dims) => DeclVerilogArr(kind, id, dims)
      case _                       => unreachable
    }
    override def visitDeclArr(ctx: DeclArrContext) = {
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type())

      val id = ctx.IDENTIFIER.text

      val exprVisitor = new ExprVisitor(Some(self), typedefs)
      val indices = exprVisitor(ctx.es)

      kind match {
        case x: ScalarType => DeclArr(x, id, indices)
        case s: Struct => {
          Message.error(ctx, s"Arrays must be declared with scalar types, not struct '${s.name}'")
          DeclArr(IntType(false, 1), id, indices);
        }
        case VoidType => unreachable
      }
    }
    override def visitDeclVarNoInit(ctx: DeclVarNoInitContext) = {
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type())
      DeclVar(kind, ctx.IDENTIFIER, None)
    }
    override def visitDeclVarInit(ctx: DeclVarInitContext) = {
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type())
      DeclVar(kind, ctx.IDENTIFIER, Some(ErrorExpr))
    }
  }

  // Walk the parse tree, extract declared names and build the variable scopes.
  // Note that initialiser expressions are not parsed just yet, as names will be
  // re-mapped lated
  private[this] object BuildScopes extends VScalarVisitor[Unit] {
    override def defaultResult = ()

    // Create new scope for blocks
    override def visitBlockStmt(ctx: BlockStmtContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for 'loop' loop
    override def visitLoopStmt(ctx: LoopStmtContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for 'while' loop
    override def visitWhileStmt(ctx: WhileStmtContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for 'for' loop
    override def visitForStmt(ctx: ForStmtContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for 'do' loop
    override def visitDoStmt(ctx: DoStmtContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for fence functon
    override def visitFenceFunction(ctx: FenceFunctionContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for fsm bodies
    override def visitEntityFSM(ctx: EntityFSMContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for verilog bodies
    override def visitEntityVerilog(ctx: EntityVerilogContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for network bodies
    override def visitEntityNetwork(ctx: EntityNetworkContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Insert task declarations
    override def visitTaskDeclOut(ctx: TaskDeclOutContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitTaskDeclIn(ctx: TaskDeclInContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitTaskDeclConst(ctx: TaskDeclConstContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitTaskDeclParam(ctx: TaskDeclParamContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitTaskDeclPipeline(ctx: TaskDeclPipelineContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitTaskDeclVerilog(ctx: TaskDeclVerilogContext) = insert(ctx, Left(DeclExtractor(ctx)))

    // Insert ordinary Decl
    override def visitDeclVarNoInit(ctx: DeclVarNoInitContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitDeclVarInit(ctx: DeclVarInitContext) = insert(ctx, Left(DeclExtractor(ctx)))
    override def visitDeclArr(ctx: DeclArrContext) = insert(ctx, Left(DeclExtractor(ctx)))

    // Insert function names and create new scope
    override def visitFunction(ctx: FunctionContext) = {
      insert(ctx, Right(ctx.IDENTIFIER.text))
      create(ctx)
      visitChildren(ctx)
    }
  }

  // Create root scope
  scopes(root) = mutable.Map[String, Option[Item]]().withDefaultValue(None)

  // Add all predefined names
  //  for (id <- List("go", "state", "call_stack", "call_depth")) {
  //    insert(root, id)
  //  }

  // Built the scopes
  BuildScopes(root)

  // Re-map variables with multiplicity > 1
  for (nameMap <- scopes.values; (name, item) <- nameMap if multiplicity(name) > 1) {
    item match {
      case Some(Item(Left(decl), ctx)) if (name == decl.id) => {
        val newId = decl.id + s"_L${ctx.loc.line}"
        val newDecl = decl match {
          case d: DeclVar => d.copy(id = newId)
          case d: DeclArr => d.copy(id = newId)
          case _          => unreachable
        }
        nameMap(name) = Some(Item(Left(newDecl), ctx))
      }
      case _ => ()
    }
  }

  // Parse initialiser expressions in the context of new names
  private[this] val exprVisitor = new ExprVisitor(Some(self), typedefs)
  for (nameMap <- scopes.values; (name, item) <- nameMap) {
    item match {
      case Some(Item(Left(decl), ctx)) => {
        val newDecl = decl match {
          case d @ DeclVar(_, _, Some(init)) => {
            Some(d.copy(init = Some(exprVisitor(ctx.asInstanceOf[DeclVarInitContext].expr))))
          }
          case d @ DeclParam(_, _, init) => {
            Some(d.copy(init = exprVisitor(ctx.asInstanceOf[TaskDeclParamContext].expr)))
          }
          case d @ DeclConst(_, _, init) => {
            Some(d.copy(init = exprVisitor(ctx.asInstanceOf[TaskDeclConstContext].expr)))
          }
          case _ => None
        }
        if (newDecl.isDefined) {
          nameMap(name) = Some(Item(Left(newDecl.get), ctx))
        }
      }
      case Some(Item(Right(decl), loc)) => ()
      case None                         =>
    }
  }

  // Map from parse tree node to Decl, if the node is a Decl construct
  private[this] val decls = {
    for {
      nameMap <- scopes.values
      (name, itemOpt) <- nameMap
      Some(item) = itemOpt
      if (item.decl.isLeft)
      Left(decl) = item.decl
    } yield {
      item.ctx -> decl
    }
  }.toMap

  // Look up Decl at this node
  def apply(ctx: ParserRuleContext): Decl = decls(ctx)

  // Look up name in scope of node ctx
  def apply(ctx: ParserRuleContext, name: String): Either[Decl, String] = {
    find(ctx)(name) match {
      case Some(Item(decl, _)) => decl
      case None                => Message.fatal(ctx, s"Unknown identifier '$name'")
    }
  }
}
