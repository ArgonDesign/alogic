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
import com.argondesign.alogic.core.Types.TypeRecord
import com.argondesign.alogic.core.Types.TypeStack
import com.argondesign.alogic.core.Types.TypeType
import com.argondesign.alogic.core.enums.UninitializedLocals
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable.ListBuffer
import scala.util.Random

final class ConvertCtrlFuncLocals(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private val extraDecls = ListBuffer[Decl]()

  private val extraDefns = ListBuffer[Defn]()

  private var rng: Random = _

  override def start(tree: Tree): Unit = tree match {
    case Defn(symbol) => rng = new Random(symbol.name.foldLeft(0)(_ ^ _))
    case _: Decl      =>
    case _            => unreachable
  }

  private def getDefaultInitializer(kind: Type): Option[Expr] = {
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

  // The storage structure for local variables of the current control function
  private var lSymbolOpt: Option[Symbol] = None
  // Map from the local variables of the current control function to the field
  // name in the local storage structure
  private var locals: Map[Symbol, String] = Map.empty

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Analyse local variables
    case DefnFunc(symbol, _, body) if symbol.kind.isCtrlFunc =>
      // Gather locals, excluding known combinational signals
      val localSymbols = List from {
        body.iterator flatMap {
          _ flatCollect {
            case DeclVar(symbol, _) => Option.unless(symbol.attr.combSignal.isSet)(symbol)
            case DeclVal(symbol, _) => Option.unless(symbol.attr.combSignal.isSet)(symbol)
            case _: DeclStatic      => None
            case _: Decl            => unreachable
          }
        }
      }
      // Create the name map
      val mSymbols = localSymbols map { symbol => symbol.dup tap { _.kind = symbol.kind } }
      RenameSymbols.makeNamesUnique(mSymbols)
      locals = Map from { localSymbols.iterator zip (mSymbols.iterator map { _.name }) }
      // Create the storage structure
      lSymbolOpt = Option.when(localSymbols.nonEmpty) {
        // Create the locals structure type
        val sSymbol = cc.newSymbol(s"${symbol.name}${cc.sep}locals_t", symbol.loc)
        val sKind = TypeRecord(sSymbol, mSymbols)
        sSymbol.kind = TypeType(sKind)
        extraDecls append (sSymbol.mkDecl regularize tree.loc)
        extraDefns append (sSymbol.mkDefn regularize tree.loc)
        // Create the locals variable/stack
        cc.newSymbol(s"${symbol.name}${cc.sep}locals", symbol.loc) tap { lSymbol =>
          lSymbol.kind = {
            val recLimit = symbol.attr.recLimit.value.value.get.toInt
            if (recLimit > 1) TypeStack(sKind, recLimit) else sKind
          }
          extraDecls append (lSymbol.mkDecl regularize tree.loc)
          extraDefns append (lSymbol.mkDefn regularize tree.loc)
        }
      }
      // Add total number of local variable bits to stats
      cc.stats((symbol.sourceName, "local-bits")) = localSymbols.foldLeft(0)(_ + _.kind.width.toInt)
      //
      None

    // Extract Decl/Defn in statement position that is not in the local storage
    case StmtDecl(decl) if !(locals contains decl.symbol) =>
      extraDecls append decl
      None

    case StmtDefn(defn) if !(locals contains defn.symbol) =>
      extraDefns append defn
      None

    //
    case _ => None
  }

  private def selLocal(name: String): Expr = {
    val lSymbol = lSymbolOpt.get
    lSymbol.kind match {
      case _: TypeStack => ExprSym(lSymbol) sel "top" sel name
      case _            => ExprSym(lSymbol) sel name
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Drop StmtDecls
    ////////////////////////////////////////////////////////////////////////////

    case _: StmtDecl => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Drop StmtDefns, assign initializer if any
    ////////////////////////////////////////////////////////////////////////////

    case StmtDefn(defn) =>
      defn match {
        case _: DefnVar | _: DefnVal =>
          defn.initializer orElse getDefaultInitializer(defn.symbol.kind) map { init =>
            StmtAssign(
              locals.get(defn.symbol) map selLocal getOrElse ExprSym(defn.symbol),
              init
            ) regularize tree.loc
          } getOrElse Stump
        case _ => Stump
      }

    ////////////////////////////////////////////////////////////////////////////
    // Push locals on function entry
    ////////////////////////////////////////////////////////////////////////////

    case defn @ DefnFunc(symbol, _, body)
        if symbol.kind.isCtrlFunc && (lSymbolOpt exists { _.kind.isStack }) =>
      val push = StmtExpr(ExprSym(lSymbolOpt.get) sel "push" call Nil) regularize tree.loc
      TypeAssigner(defn.copy(body = push :: body) withLoc defn.loc)

    ////////////////////////////////////////////////////////////////////////////
    // Pop locals on function return
    ////////////////////////////////////////////////////////////////////////////

    case stmt @ StmtReturn(false, _) if lSymbolOpt exists { _.kind.isStack } =>
      Thicket(
        List(
          StmtExpr(ExprSym(lSymbolOpt.get) sel "pop" call Nil) regularize tree.loc,
          stmt
        )
      )

    ////////////////////////////////////////////////////////////////////////////
    // Replace references to locals with references to the local storage
    ////////////////////////////////////////////////////////////////////////////

    case ExprSym(symbol) =>
      locals.get(symbol) map { name => selLocal(name) regularize tree.loc } getOrElse tree

    ////////////////////////////////////////////////////////////////////////////
    // Add entity extra Decl/Defn pairs, convert all to DeclVar/DefnVar
    ////////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity if extraDecls.nonEmpty =>
      val newDecls = List from {
        decl.decls.iterator concat {
          extraDecls.iterator map {
            case d: DeclVar    => d
            case d: DeclVal    => TypeAssigner(DeclVar(d.symbol, d.spec) withLoc d.loc)
            case d: DeclStatic => TypeAssigner(DeclVar(d.symbol, d.spec) withLoc d.loc)
            case d: DeclStack  => d
            case d: DeclRecord => d
            case _             => unreachable
          }
        }
      }
      TypeAssigner(decl.copy(decls = newDecls) withLoc tree.loc)

    case defn: DefnEntity if extraDefns.nonEmpty =>
      val newDefns = extraDefns.iterator map {
        case d: DefnVar    => TypeAssigner(DefnVar(d.symbol, None) withLoc d.loc)
        case d: DefnVal    => TypeAssigner(DefnVar(d.symbol, None) withLoc d.loc)
        case d: DefnStatic => TypeAssigner(DefnVar(d.symbol, d.initOpt) withLoc d.loc)
        case d: DefnStack  => d
        case d: DefnRecord => d
        case _             => unreachable
      } map { d =>
        TypeAssigner(EntDefn(d) withLoc d.loc)
      }
      val newBody = List.from(defn.body.iterator ++ newDefns)
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    //
    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = tree visit {
    case node: StmtDecl   => throw Ice(node, "StmtDecl remains")
    case node: StmtDefn   => throw Ice(node, "StmtDefn remains")
    case node: DeclVal    => throw Ice(node, "DeclVal remains")
    case node: DefnVal    => throw Ice(node, "DefnVal remains")
    case node: DeclStatic => throw Ice(node, "DeclStatic remains")
    case node: DefnStatic => throw Ice(node, "DefnStatic remains")
  }

}

object ConvertCtrlFuncLocals extends EntityTransformerPass(declFirst = false) {
  val name = "convert-ctrl-func-locals"

  override protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new ConvertCtrlFuncLocals
}
