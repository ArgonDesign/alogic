////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Do:
// - Convert port flow control to stall statements
// - Split ports with flow control into constituent signals
// - Lower output storage slices into output slice instances
//
// Do not:
// - Update Connects yet
// - Replace naked port references yet
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.SliceFactory
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerFlowControlA(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  private[this] def boolType(loc: Loc): TypeUInt = {
    val one = Expr(1) withLoc loc
    TypeAssigner(one)
    TypeUInt(one)
  }

  private[this] val fcn = FlowControlTypeNone
  private[this] val stw = StorageTypeWire

  override def enter(tree: Tree): Unit = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeNone
    ////////////////////////////////////////////////////////////////////////////

    case Decl(Sym(symbol: TermSymbol), TypeIn(_, FlowControlTypeNone), _) => {
      symbol setAttr "fcn"
    }

    case Decl(Sym(symbol: TermSymbol), TypeOut(_, FlowControlTypeNone, _), _) => {
      symbol setAttr "fcn"
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeValid
    ////////////////////////////////////////////////////////////////////////////

    case Decl(Sym(symbol: TermSymbol), TypeIn(kind, FlowControlTypeValid), _) => {
      // Allocate payload and valid signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeIn(kind, fcn))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeIn(boolType(loc), fcn))
      val newSymbols = if (kind != TypeVoid) (pSymbol, vSymbol) else (ErrorSymbol, vSymbol)
      // Add new Symbols as attributes
      symbol.setAttr("fcv", newSymbols)
      symbol.setAttr("expanded-port")
    }

    case Decl(Sym(symbol: TermSymbol), TypeOut(kind, FlowControlTypeValid, st), _) => {
      // Allocate payload and valid signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fcn, st))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fcn, st))
      val newSymbols = if (kind != TypeVoid) (pSymbol, vSymbol) else (ErrorSymbol, vSymbol)
      // Add new Symbols as attributes
      symbol.setAttr("fcv", newSymbols)
      symbol.setAttr("expanded-port")
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeReady
    ////////////////////////////////////////////////////////////////////////////

    case Decl(Sym(symbol: TermSymbol), TypeIn(kind, FlowControlTypeReady), _) => {
      // Allocate payload, valid and ready signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      val rName = pName + "_ready"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeIn(kind, fcn))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeIn(boolType(loc), fcn))
      val rSymbol = cc.newTermSymbol(rName, loc, TypeOut(boolType(loc), fcn, stw))
      val newSymbols = if (kind != TypeVoid) {
        (pSymbol, vSymbol, rSymbol)
      } else {
        (ErrorSymbol, vSymbol, rSymbol)
      }
      // Add new Symbols as attributes
      symbol.setAttr("fcr", newSymbols)
      symbol.setAttr("expanded-port")
    }

    case Decl(Sym(symbol: TermSymbol), TypeOut(kind, FlowControlTypeReady, st), _) => {
      // Allocate payload, valid and ready signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      val rName = pName + "_ready"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fcn, stw))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fcn, stw))
      val rSymbol = cc.newTermSymbol(rName, loc, TypeIn(boolType(loc), fcn))
      val newSymbols = if (kind != TypeVoid) {
        (pSymbol, vSymbol, rSymbol)
      } else {
        (ErrorSymbol, vSymbol, rSymbol)
      }
      // Add new Symbols as attributes
      symbol.setAttr("fcr", newSymbols)
      symbol.setAttr("expanded-port")
      // If output slices are required, construct them
      if (st != StorageTypeWire) {
        val StorageTypeSlices(slices) = st
        // TODO: mark inline
        val eName = entitySymbol.name + "__oslice__" + pName
        val sliceEntity: Entity = SliceFactory(slices, eName, loc, kind)
        val Sym(sliceEntitySymbol) = sliceEntity.ref
        val instanceSymbol = {
          val iName = "oslice__" + pName
          cc.newTermSymbol(iName, loc, sliceEntitySymbol.denot.kind)
        }
        // Add slice entity and instance symbol as attributes
        symbol.setAttr("oslice", (sliceEntity, instanceSymbol))
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeAccept
    ////////////////////////////////////////////////////////////////////////////

    case Decl(Sym(symbol: TermSymbol), TypeIn(kind, FlowControlTypeAccept), _) => {
      // Allocate payload, valid and accept signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      val aName = pName + "_accept"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeIn(kind, fcn))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeIn(boolType(loc), fcn))
      val aSymbol = cc.newTermSymbol(aName, loc, TypeOut(boolType(loc), fcn, stw))
      val newSymbols = if (kind != TypeVoid) {
        (pSymbol, vSymbol, aSymbol)
      } else {
        (ErrorSymbol, vSymbol, aSymbol)
      }
      // Add new Symbols as attributes
      symbol.setAttr("fca", newSymbols)
      symbol.setAttr("expanded-port")
    }

    case Decl(Sym(symbol: TermSymbol), TypeOut(kind, FlowControlTypeAccept, st), _) => {
      assert(st == StorageTypeWire)
      // Allocate payload, valid and ready signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      val aName = pName + "_accept"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fcn, stw))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fcn, stw))
      val aSymbol = cc.newTermSymbol(aName, loc, TypeIn(boolType(loc), fcn))
      val newSymbols = if (kind != TypeVoid) {
        (pSymbol, vSymbol, aSymbol)
      } else {
        (ErrorSymbol, vSymbol, aSymbol)
      }
      // Add new Symbols as attributes
      symbol.setAttr("fca", newSymbols)
      symbol.setAttr("expanded-port")
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeReady
    ////////////////////////////////////////////////////////////////////////////

    case _: Stmt => {
      // Whenever we enter a new statement, add a new buffer to
      // store potential extra statements
      extraStmts.push(ListBuffer())
    }

    case _ =>
  }

  private[this] def stall(expr: Expr) = StmtIf(expr, StmtStall(), None)
  private[this] def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      // We used the Error symbol for void port payloads, now replace
      // the corresponding statement with an empty statement
      case StmtExpr(ExprRef(Sym(ErrorSymbol))) => StmtBlock(Nil)

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "write"), args)) => {
        symbol.getAttr[Boolean]("fcn").map { _ =>
          StmtAssign(ref, args.head)
        } orElse symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (pSymbol, vSymbol) =>
            val vAssign = assignTrue(ExprRef(Sym(vSymbol)))
            if (pSymbol != ErrorSymbol) {
              val pAssign = StmtAssign(ExprRef(Sym(pSymbol)), args.head)
              StmtBlock(List(pAssign, vAssign))
            } else {
              vAssign
            }
        } orElse symbol.getAttr[(Entity, TermSymbol)]("oslice").map {
          case (_, iSymbol) =>
            val pSymbol = symbol.attr[(Symbol, TermSymbol, TermSymbol)]("fcr")._1
            val iRef = ExprRef(Sym(iSymbol))
            val vAssign = assignTrue(iRef select "u_valid")
            val rStall = stall(~(iRef select "u_ready"))
            if (pSymbol != ErrorSymbol) {
              val pAssign = StmtAssign(iRef select "u_payload", args.head)
              StmtBlock(List(pAssign, vAssign, rStall))
            } else {
              StmtBlock(List(vAssign, rStall))
            }
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
          case (pSymbol, vSymbol, aSymbol) =>
            val vAssign = assignTrue(ExprRef(Sym(vSymbol)))
            val aStall = stall(!ExprRef(Sym(aSymbol)))
            if (pSymbol != ErrorSymbol) {
              val pAssign = StmtAssign(ExprRef(Sym(pSymbol)), args.head)
              StmtBlock(List(pAssign, vAssign, aStall))
            } else {
              StmtBlock(List(vAssign, aStall))
            }
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "wait"), args)) => {
        symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (_, vSymbol) =>
            stall(!ExprRef(Sym(vSymbol)))
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
          case (_, vSymbol, _) =>
            stall(!ExprRef(Sym(vSymbol)))
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "flush"), args)) => {
        symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (_, vSymbol) =>
            stall(ExprRef(Sym(vSymbol)))
        } orElse symbol.getAttr[(Entity, TermSymbol)]("oslice").map {
          case (_, iSymbol) =>
            val iRef = ExprRef(Sym(iSymbol))
            stall(~(iRef select "empty"))
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "read"), Nil) => {
        symbol.getAttr[Boolean]("fcn").map { _ =>
          ref
        } orElse symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (pSymbol, vSymbol) =>
            extraStmts.top append stall(!ExprRef(Sym(vSymbol)))
            ExprRef(Sym(pSymbol))
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
          case (pSymbol, vSymbol, rSymbol) =>
            extraStmts.top append stall(!ExprRef(Sym(vSymbol)))
            extraStmts.top append assignTrue(ExprRef(Sym(rSymbol)))
            ExprRef(Sym(pSymbol))
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
          case (pSymbol, vSymbol, aSymbol) =>
            extraStmts.top append stall(!ExprRef(Sym(vSymbol)))
            extraStmts.top append assignTrue(ExprRef(Sym(aSymbol)))
            ExprRef(Sym(pSymbol))
        } getOrElse {
          tree
        }
      }

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "valid"), Nil) => {
        symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (_, vSymbol) => ExprRef(Sym(vSymbol))
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
          case (_, vSymbol, _) => ExprRef(Sym(vSymbol))
        } getOrElse {
          tree
        }
      }

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "empty"), Nil) => {
        symbol.getAttr[(Entity, TermSymbol)]("oslice").map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "empty"
        } getOrElse {
          tree
        }
      }

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "full"), Nil) => {
        symbol.getAttr[(Entity, TermSymbol)]("oslice").map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "full"
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add declarations of the expanded symbols
      //////////////////////////////////////////////////////////////////////////

      case decl @ Decl(Sym(symbol: TermSymbol), _, _) => {
        // Note: We also leave the declaration of the original symbol, as
        // Connect instances have not been rewritten yet. These will be fixed
        // up in a later pass as they required all entities to have been
        // converted before we can type port references
        symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (pSymbol, vSymbol) =>
            val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
            val newDecls = if (pSymbol != ErrorSymbol) {
              val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
              List(pDecl, vDecl)
            } else {
              List(vDecl)
            }
            Thicket(decl :: newDecls)
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
          case (pSymbol, vSymbol, rSymbol) =>
            val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
            val rDecl = Decl(Sym(rSymbol), rSymbol.denot.kind, None)
            val newDecls = if (pSymbol != ErrorSymbol) {
              val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
              List(pDecl, vDecl, rDecl)
            } else {
              List(vDecl, rDecl)
            }
            Thicket(decl :: newDecls)
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
          case (pSymbol, vSymbol, aSymbol) =>
            val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
            val aDecl = Decl(Sym(aSymbol), aSymbol.denot.kind, None)
            val newDecls = if (pSymbol != ErrorSymbol) {
              val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
              List(pDecl, vDecl, aDecl)
            } else {
              List(vDecl, aDecl)
            }
            Thicket(decl :: newDecls)
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add storage slice entities
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        val pairs = entity.declarations collect {
          case Decl(Sym(symbol), _, _) if symbol.hasAttr("oslice") => {
            (symbol, symbol.attr[(Entity, TermSymbol)]("oslice"))
          }
        }

        val instances = for ((_, (entity, instance)) <- pairs) yield {
          Instance(Sym(instance), entity.ref, Nil, Nil)
        }

        val connects = pairs flatMap {
          case (symbol, (_, iSymbol)) =>
            val (pSymbol, vSymbol, rSymbol) = symbol.attr[(Symbol, TermSymbol, TermSymbol)]("fcr")
            val iRef = ExprRef(Sym(iSymbol))
            lazy val pConn = Connect(iRef select "d_payload", List(ExprRef(Sym(pSymbol))))
            val vConn = Connect(iRef select "d_valid", List(ExprRef(Sym(vSymbol))))
            val rConn = Connect(ExprRef(Sym(rSymbol)), List(iRef select "d_ready"))
            if (pSymbol != ErrorSymbol) {
              List(pConn, vConn, rConn)
            } else {
              List(vConn, rConn)
            }
        }

        // Remove fcn and oslice attributes, they are no longer needed
        entity.declarations foreach {
          case Decl(Sym(symbol), _, _) => {
            symbol delAttr "fcn"
            symbol delAttr "oslice"
          }
          case _ => unreachable
        }

        // Update type of entity to include the new ports. We also leave the old
        // un-converted port for now, as Connect instances have not been updated
        // yet.
        val portSymbols = entity.declarations collect {
          case Decl(Sym(symbol: TermSymbol), _: TypeIn, _)  => symbol
          case Decl(Sym(symbol: TermSymbol), _: TypeOut, _) => symbol
        }

        val TypeEntity(name, _, Nil) = entitySymbol.denot.kind
        val newKind = TypeEntity(name, portSymbols, Nil)
        entitySymbol withDenot entitySymbol.denot.copy(kind = newKind)

        val entities = pairs map { _._2._1 }
        val thisEntity = entity.copy(
          instances = instances ::: entity.instances,
          connects = connects ::: entity.connects
        ) withVariant entity.variant

        Thicket(thisEntity :: entities)
      }

      case _ => tree
    }

    // Emit any extra statement with this statement
    val result2 = result match {
      case stmt: Stmt => {
        val extra = extraStmts.top
        if (extra.isEmpty) {
          stmt
        } else {
          extra append stmt
          StmtBlock(extra.toList)
        }
      } followedBy {
        extraStmts.pop()
      }
      case _ => result
    }

    // If we did modify the node, regularize it
    if (result2 ne tree) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    tree visit {
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeOut] => {
        cc.ice(node, s"Output port .${sel} remains")
      }
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeIn] => {
        cc.ice(node, s"Input port .${sel} remains")
      }
    }
  }

}
