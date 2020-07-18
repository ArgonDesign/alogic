////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Convert local variables in control functions to entity variables
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.enums.UninitializedLocals
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

final class ConvertLocals(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private[this] val localDecls = ListBuffer[Decl]()

  private[this] val localDefns = ListBuffer[Defn]()

  private[this] var rng: Random = _

  override def start(tree: Tree): Unit = tree match {
    case Defn(symbol) => rng = new Random(symbol.name.foldLeft(0)(_ ^ _))
    case _: Decl      =>
    case _            => unreachable
  }

  private[this] def getDefaultInitializer(kind: Type): Option[Expr] = {
    val width = kind.width.toInt
    Option.when(width > 0 && cc.settings.uninitialized != UninitializedLocals.None) {
      val signed = kind.isSigned
      cc.settings.uninitialized match {
        case UninitializedLocals.None  => unreachable
        case UninitializedLocals.Zeros => ExprInt(signed, width, 0)
        case UninitializedLocals.Ones =>
          if (signed) {
            ExprInt(signed = true, width, -1)
          } else {
            ExprInt(signed = false, width, (BigInt(1) << width) - 1)
          }
        case UninitializedLocals.Random =>
          (width, signed) match {
            case (1, true)  => ExprInt(signed = true, 1, -BigInt(1, rng))
            case (1, false) => ExprInt(signed = false, 1, BigInt(1, rng))
            case (n, true)  => ExprInt(signed = true, n, BigInt(n, rng) - (BigInt(1) << (n - 1)))
            case (n, false) => ExprInt(signed = false, n, BigInt(n, rng))
          }
      }
    }
  }

  // The locals in the current control function
  private var locals = SortedSet.empty[Symbol]
  // The recLimit of the current control function
  private var recLimit: Option[Int] = None

  // Repalcing all locals
  override protected def replace(symbol: Symbol): Boolean = locals(symbol)

  override def enter(tree: Tree): Option[Tree] = tree match {
    case DefnFunc(symbol, _, body) if symbol.kind.isCtrlFunc =>
      // Gather locals
      locals = SortedSet from {
        body.iterator flatMap {
          _ collect {
            case DeclVar(symbol, _) => symbol
            case DeclVal(symbol, _) => symbol
            case _: Decl            => unreachable
          }
        }
      }
      // Hold on to recLimit
      recLimit = Some(symbol.attr.recLimit.value.value.get.toInt)
      //
      None

    case _ => None
  }

  private val initializers = mutable.Map[Symbol, Expr]()

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Convert locals to variables
    ////////////////////////////////////////////////////////////////////////////

    case DeclVar(symbol, spec) if orig.get(symbol) exists locals =>
      recLimit match {
        case Some(1) => tree
        case Some(n) => DeclStack(symbol, spec, Expr(n)) regularize tree.loc
        case None    => unreachable
      }

    case DefnVar(symbol, initOpt) if orig.get(symbol) exists locals =>
      initOpt foreach { initializers(symbol) = _ }
      recLimit match {
        case Some(1) => DefnVar(symbol, None) regularize tree.loc
        case Some(_) => DefnStack(symbol) regularize tree.loc
        case None    => unreachable
      }

    case DeclVal(symbol, spec) if orig.get(symbol) exists locals =>
      recLimit match {
        case Some(1) => DeclVar(symbol, spec) regularize tree.loc
        case Some(n) => DeclStack(symbol, spec, Expr(n)) regularize tree.loc
        case None    => unreachable // Can only appear in control function
      }

    case DefnVal(symbol, init) if orig.get(symbol) exists locals =>
      initializers(symbol) = init
      recLimit match {
        case Some(1) => DefnVar(symbol, None) regularize tree.loc
        case Some(_) => DefnStack(symbol) regularize tree.loc
        case None    => unreachable // Can only appear in control function
      }

    ////////////////////////////////////////////////////////////////////////////
    // Pull StmtDecl to entity
    ////////////////////////////////////////////////////////////////////////////

    case StmtDecl(decl: DeclVar) =>
      localDecls append decl
      Stump

    case StmtDecl(decl: DeclStack) =>
      localDecls append decl
      Stump

    case _: StmtDecl => unreachable

    ////////////////////////////////////////////////////////////////////////////
    // Pull StmtDefn to entity, assign initializer if any
    ////////////////////////////////////////////////////////////////////////////

    case StmtDefn(defn @ DefnVar(symbol, None)) =>
      localDefns append defn
      initializers.get(symbol) orElse getDefaultInitializer(symbol.kind) match {
        case Some(init) => StmtAssign(ExprSym(symbol), init) regularize tree.loc
        case None       => Stump
      }

    case StmtDefn(defn @ DefnStack(symbol)) =>
      localDefns append defn
      initializers.get(symbol) orElse getDefaultInitializer(symbol.kind.asStack.kind) match {
        case Some(init) => StmtAssign(ExprSym(symbol) sel "top", init) regularize tree.loc
        case None       => Stump
      }

    case _: StmtDefn => unreachable

    ////////////////////////////////////////////////////////////////////////////
    // Push all locals on function entry
    ////////////////////////////////////////////////////////////////////////////

    case defn @ DefnFunc(_, _, body) if locals.nonEmpty && recLimit.get > 1 => {
        val newBody = locals.iterator.foldLeft(body) {
          case (tail, oldSymbol) =>
            val symbol = repl(oldSymbol).get
            val call = ExprSym(symbol) sel "push" call Nil
            val head = StmtExpr(call) regularize symbol.loc
            head :: tail
        }
        TypeAssigner(defn.copy(body = newBody) withLoc defn.loc)
      } tap { _ =>
        locals = SortedSet.empty
        recLimit = None
      }

    ////////////////////////////////////////////////////////////////////////////
    // Pop all locals on function return
    ////////////////////////////////////////////////////////////////////////////

    case stmt @ StmtReturn(false, _) if locals.nonEmpty && recLimit.get > 1 =>
      Thicket {
        locals.iterator.foldLeft(List[Stmt](stmt)) {
          case (tail, oldSymbol) =>
            val symbol = repl(oldSymbol).get
            val call = ExprSym(symbol) sel "pop" call Nil
            val head = StmtExpr(call) regularize symbol.loc
            head :: tail
        }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Replace references to stacked locals with references to the top
    ////////////////////////////////////////////////////////////////////////////

    case expr @ ExprSym(symbol) if (orig.get(symbol) exists locals) && (recLimit.get > 1) =>
      expr sel "top"

    ////////////////////////////////////////////////////////////////////////////
    // Add entity extra Decl/Defn pairs
    ////////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity if localDecls.nonEmpty =>
      val newDecls = List.from(decl.decls.iterator ++ localDecls.iterator)
      TypeAssigner(decl.copy(decls = newDecls) withLoc tree.loc)

    case defn: DefnEntity if localDefns.nonEmpty =>
      val newDefns = localDefns.iterator map { d =>
        TypeAssigner(EntDefn(d) withLoc d.loc)
      }
      val newBody = List.from(defn.body.iterator ++ newDefns)
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: StmtDecl => throw Ice(node, "Local declaration remains")
    }
  }

}

object ConvertLocals extends EntityTransformerPass(declFirst = false) {
  val name = "convert-locals"

  override protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new ConvertLocals
}
