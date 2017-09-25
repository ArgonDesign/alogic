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

  // A named pair of declaration and the location of the declaration. For data declarations,
  // we use Left[Declaration], for function declarations, we use Right[Declaration]
  private[this] case class Item(decl: Either[Declaration, String], ctx: ParserRuleContext)

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
  private[this] def insert(ctx: ParserRuleContext, name: String, decl: Either[Declaration, String]): Unit = {
    val scope = find(ctx)
    if (scope contains name) {
      val Some(Item(_, pctx)) = scope(name)
      Message.error(ctx, s"Multiple declarations of name '$name' ...")
      Message.error(ctx, s"... previous declaration at: ${pctx.loc}")
    } else {
      scope(name) match {
        case Some(Item(_, pctx)) => {
          Message.warning(ctx, s"Declaration of '$name' hides previous declaration of same name at ...")
          Message.warning(ctx, s"... ${pctx.loc}")
        }
        case None => ()
      }
    }
    scope(name) = Some(Item(decl, ctx))
    multiplicity(name) += {
      decl match {
        case Left(_: VarDeclaration)   => 1
        case Left(_: ArrayDeclaration) => 1
        case _                         => 0
      }
    }
  }

  // Walk the parse tree, extract declared names and build the variable scopes.
  // Note that initialiser expressions are not parsed just yet, as names will be
  // re-mapped lated
  private[this] object BuildScopes extends VScalarVisitor[Unit] {
    // Get VarRef from parse tree
    object VarRefVisitor extends VScalarVisitor[VarRef] {
      object DottedNameVisitor extends VScalarVisitor[DottedName] {
        override def visitDotted_name(ctx: Dotted_nameContext) = DottedName(ctx.es.toList.map(_.text))
      }
      override def visitVarRef(ctx: VarRefContext) = DottedNameVisitor(ctx.dotted_name)
      override def visitVarRefIndex(ctx: VarRefIndexContext) = {
        val exprVisitor = new ExprVisitor(Some(self), typedefs)
        ArrayLookup(DottedNameVisitor(ctx.dotted_name), exprVisitor(ctx.es))
      }
    }

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
    override def visitTaskFSM(ctx: TaskFSMContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for verilog bodies
    override def visitTaskVerilog(ctx: TaskVerilogContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Create new scope for network bodies
    override def visitNetwork(ctx: NetworkContext) = {
      create(ctx)
      visitChildren(ctx)
    }

    // Insert special task declarations
    override def visitTaskDeclOut(ctx: TaskDeclOutContext) = {
      val fctype = FlowControlTypeVisitor(ctx.flow_control_type)
      val kind = PortTypeVisitor(ctx.port_type)
      val id = ctx.IDENTIFIER.text
      val stype = StorageTypeVisitor(ctx.storage_type)
      val decl = OutDeclaration(fctype, kind, id, stype)
      insert(ctx, id, Left(decl))
    }
    override def visitTaskDeclIn(ctx: TaskDeclInContext) = {
      val fctype = FlowControlTypeVisitor(ctx.flow_control_type)
      val kind = PortTypeVisitor(ctx.port_type)
      val id = ctx.IDENTIFIER.text
      val decl = InDeclaration(fctype, kind, id)
      insert(ctx, id, Left(decl))
    }
    override def visitTaskDeclConst(ctx: TaskDeclConstContext) = {
      val id = ctx.IDENTIFIER.text
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type) match {
        case x: ScalarType => x
        case x             => Message.error(ctx, s"Constant '${id}' must be declared with scalar type"); IntType(false, 1);
      }
      val decl = ConstDeclaration(kind, id, ErrorExpr)
      insert(ctx, id, Left(decl))
    }
    override def visitTaskDeclParam(ctx: TaskDeclParamContext) = {
      val id = ctx.IDENTIFIER.text
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type) match {
        case x: ScalarType => x
        case x             => Message.error(ctx, s"Parameter '${id}' must be declared with scalar type"); IntType(false, 1);
      }
      val decl = ParamDeclaration(kind, id, ErrorExpr)
      insert(ctx, id, Left(decl))
    }
    override def visitTaskDeclPipeline(ctx: TaskDeclPipelineContext) = {
      val id = ctx.IDENTIFIER.text
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type);
      val decl = PipelineVarDeclaration(kind, id)
      insert(ctx, id, Left(decl))
    }
    override def visitTaskDeclVerilog(ctx: TaskDeclVerilogContext) = {
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type)
      val vref = VarRefVisitor(ctx.var_ref)
      val decl = vref match {
        case DottedName(name :: Nil) => VerilogVarDeclaration(kind, name)
        case ArrayLookup(DottedName(name :: Nil), index) => {
          kind match {
            case x: ScalarType => VerilogArrayDeclaration(x, name, index)
            case s: Struct => {
              Message.error(ctx, s"Arrays must be declared with scalar type, not struct '${s.name}'")
              ArrayDeclaration(IntType(false, 1), name, index);
            }
            case VoidType => unreachable
          }
        }
        case _ => unreachable
      }
      insert(ctx, decl.id, Left(decl))
    }

    // Insert regular declaration
    override def visitDeclNoInit(ctx: DeclNoInitContext) = {
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type())
      val vref = VarRefVisitor(ctx.var_ref)
      val decl = vref match {
        case DottedName(name :: Nil) => VarDeclaration(kind, name, None)
        case ArrayLookup(DottedName(name :: Nil), index) => {
          kind match {
            case x: ScalarType => ArrayDeclaration(x, name, index)
            case s: Struct => {
              Message.error(ctx, s"Arrays must be declared with scalar types, not struct '${s.name}'")
              ArrayDeclaration(IntType(false, 1), name, index);
            }
            case VoidType => unreachable
          }
        }
        case _ => unreachable
      }
      insert(ctx, decl.id, Left(decl))
    }
    override def visitDeclInit(ctx: DeclInitContext) = {
      val knownTypeVisitor = new KnownTypeVisitor(Some(self), typedefs)
      val kind = knownTypeVisitor(ctx.known_type())
      val vref = VarRefVisitor(ctx.var_ref)
      val decl = vref match {
        case DottedName(name :: Nil) => VarDeclaration(kind, name, Some(ErrorExpr))
        case ArrayLookup(DottedName(name :: Nil), _) => {
          Message.error(ctx, s"Cannot use initializer in declaration of array '${name}'")
          VarDeclaration(kind, name, None)
        }
        case _ => unreachable
      }
      insert(ctx, decl.id, Left(decl))
    }

    // Insert function names and create new scope
    override def visitFunction(ctx: FunctionContext) = {
      val id = ctx.IDENTIFIER.text
      insert(ctx, id, Right(id))
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
          case d: VarDeclaration          => d.copy(id = newId)
          case d: ParamDeclaration        => d.copy(id = newId)
          case d: ConstDeclaration        => d.copy(id = newId)
          case d: ArrayDeclaration        => d.copy(id = newId)
          case d: PipelineVarDeclaration  => d.copy(id = newId)
          case d: InDeclaration           => d.copy(id = newId)
          case d: OutDeclaration          => d.copy(id = newId)
          case d: VerilogVarDeclaration   => d.copy(id = newId)
          case d: VerilogArrayDeclaration => d.copy(id = newId)
        }
        nameMap(name) = Some(Item(Left(newDecl), ctx))
      }
      case _ => ()
    }
  }

  // Parse initialiser expressions in the context of new names
  val exprVisitor = new ExprVisitor(Some(self), typedefs)
  for (nameMap <- scopes.values; (name, item) <- nameMap) {
    item match {
      case Some(Item(Left(decl), ctx)) => {
        val newDecl = decl match {
          case d @ VarDeclaration(_, _, Some(init)) => {
            Some(d.copy(init = Some(exprVisitor(ctx.asInstanceOf[DeclInitContext].expr))))
          }
          case d @ ParamDeclaration(_, _, init) => {
            Some(d.copy(init = exprVisitor(ctx.asInstanceOf[TaskDeclParamContext].expr)))
          }
          case d @ ConstDeclaration(_, _, init) => {
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

  // Look up name in scope of node ctx
  def apply(ctx: ParserRuleContext, name: String): Either[Declaration, String] = {
    find(ctx)(name) match {
      case Some(Item(decl, _)) => decl
      case None                => Message.fatal(ctx, s"Unknown identifier '$name'")
    }
  }
}
