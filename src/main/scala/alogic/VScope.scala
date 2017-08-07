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

package alogic
import scala.collection.mutable

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.RuleContext
import org.antlr.v4.runtime.tree.RuleNode

import Antlr4Conversions._
import alogic.antlr.VParser._

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
class VScope(root: ParserRuleContext) {

  // A named pair of declared name and the location of the declaration
  private[this] case class Item(name: String, loc: Loc)

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
  private[this] def insert(ctx: ParserRuleContext, name: String, renameDuplicates: Boolean = true ): Unit = {
    val scope = find(ctx)
    if (scope contains name) {
      val Some(Item(_, loc)) = scope(name)
      Message.error(ctx, s"Multiple declarations of name '$name' ...")
      Message.error(ctx, s"... previous declaration at: ${loc}")
    } else {
      scope(name) match {
        case Some(Item(_, loc)) => {
          Message.warning(ctx, s"Declaration of '$name' hides previous declaration of same name at ...")
          Message.warning(ctx, s"... ${loc}")
        }
        case None => ()
      }
    }
    scope(name) = Some(Item(name, ctx.loc))
    if (renameDuplicates) {
      multiplicity(name) += 1
    }
  }

  // Look up name in scope of node ctx
  def apply(ctx: ParserRuleContext, name: String): String = {
    find(ctx)(name) match {
      case Some(Item(name, loc)) => if (multiplicity(name) <= 1) name else s"${name}_L${loc.line}"
      case None => {
        Message.error(ctx, s"Unknown identifier '$name'")
        s"Unknown_$name"
      }
    }
  }

  // Walk the parse tree, extract declared names and build the variable scopes
  private[this] object BuildScopes extends VScalarVisitor[Unit] {
    override def defaultResult = ()

    // Extract name from var_ref and insert into current scope
    object InsertDeclVarRef extends VScalarVisitor[Unit] {
      override def defaultResult = ()

      override def visitVarRefIndex(ctx: VarRefIndexContext) = visit(ctx.dotted_name)

      override def visitDotted_name(ctx: Dotted_nameContext) = {
        val name = ctx.es.toList.map(_.text) mkString "."

        insert(ctx, name)

        if (name contains '.') {
          Message.error(ctx, s"Declaration of scoped name is invalid '$name'")
        }
      }
    }

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
    override def visitTaskDeclOut(ctx: TaskDeclOutContext) = insert(ctx, ctx.IDENTIFIER, false)
    override def visitTaskDeclIn(ctx: TaskDeclInContext) = insert(ctx, ctx.IDENTIFIER, false)
    override def visitTaskDeclConst(ctx: TaskDeclConstContext) = insert(ctx, ctx.IDENTIFIER, false)
    override def visitTaskDeclParam(ctx: TaskDeclParamContext) = insert(ctx, ctx.IDENTIFIER, false)
    override def visitTaskDeclPipeline(ctx: TaskDeclPipelineContext) = insert(ctx, ctx.IDENTIFIER)
    override def visitTaskDeclVerilog(ctx: TaskDeclVerilogContext) = InsertDeclVarRef(ctx.var_ref)

    // Insert regular declaration
    override def visitDeclNoInit(ctx: DeclNoInitContext) = InsertDeclVarRef(ctx.var_ref)
    override def visitDeclInit(ctx: DeclInitContext) = InsertDeclVarRef(ctx.var_ref)

    // Insert function names and create new scope
    override def visitFunction(ctx: FunctionContext) = {
      insert(ctx, ctx.IDENTIFIER)
      create(ctx)
      visitChildren(ctx)
    }
  }

  // Create root scope
  scopes(root) = mutable.Map[String, Option[Item]]().withDefaultValue(None)

  // Add all predefined names
  for (id <- List("go", "state", "call_stack", "call_depth")) {
    insert(root, id)
  }

  // Built the scopes
  BuildScopes(root)
}
