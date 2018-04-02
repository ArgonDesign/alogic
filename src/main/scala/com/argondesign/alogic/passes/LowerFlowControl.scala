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
// - Convert port flow control to stall statements
// - Split ports with flow control into constituent signals
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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerFlowControl(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // Set of original symbols with FlowControlTypeNone
  private[this] val fcnSet = mutable.Set[TermSymbol]()

  // Map from original symbol with FlowControlTypeValid to the
  // corresponding optional payload and valid signals
  private[this] val fcvMap = mutable.Map[TermSymbol, (Symbol, TermSymbol)]()

  // Map from original input symbol with FlowControlTypeReady to the
  // corresponding optional payload, valid and ready signals
  private[this] val fcrIMap = mutable.Map[TermSymbol, (Symbol, TermSymbol, TermSymbol)]()

  // Map from original output symbol with FlowControlTypeReady to the
  // corresponding optional payload, valid and ready signals
  private[this] val fcrOMap = mutable.Map[TermSymbol, (Symbol, TermSymbol, TermSymbol)]()

  // Map from original output symbol with FlowControlTypeReady to the
  // corresponding storage slice entity and instance symbols
  private[this] val fcrSMap = mutable.Map[TermSymbol, (Entity, TermSymbol)]()

  // Map from original input symbol with FlowControlTypeAccept to the
  // corresponding optional payload, valid and accept signals
  private[this] val fcaMap = mutable.Map[TermSymbol, (Symbol, TermSymbol, TermSymbol)]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  private[this] def boolType(loc: Loc): TypeUInt = {
    val one = Expr(1) withLoc loc
    TypeAssigner(one)
    TypeUInt(one)
  }

  private[this] val fcn = FlowControlTypeNone
  private[this] val stw = StorageTypeWire

  private[this] var entityName: String = _

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      val Sym(symbol: TypeSymbol) = entity.ref
      entityName = symbol.denot.name.str
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeNone
    ////////////////////////////////////////////////////////////////////////////

    case Decl(Sym(symbol: TermSymbol), TypeIn(_, FlowControlTypeNone), _) => {
      fcnSet add symbol
    }

    case Decl(Sym(symbol: TermSymbol), TypeOut(_, FlowControlTypeNone, _), _) => {
      fcnSet add symbol
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
      // Add them to the map
      fcvMap(symbol) = if (kind != TypeVoid) (pSymbol, vSymbol) else (ErrorSymbol, vSymbol)
    }

    case Decl(Sym(symbol: TermSymbol), TypeOut(kind, FlowControlTypeValid, st), _) => {
      // Allocate payload and valid signals
      val loc = tree.loc
      val pName = symbol.denot.name.str
      val vName = pName + "_valid"
      lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fcn, st))
      val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fcn, st))
      // Add them to the map
      fcvMap(symbol) = if (kind != TypeVoid) (pSymbol, vSymbol) else (ErrorSymbol, vSymbol)
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
      // Add them to the map
      fcrIMap(symbol) = if (kind != TypeVoid) {
        (pSymbol, vSymbol, rSymbol)
      } else {
        (ErrorSymbol, vSymbol, rSymbol)
      }
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
      // Add them to the map
      fcrOMap(symbol) = if (kind != TypeVoid) {
        (pSymbol, vSymbol, rSymbol)
      } else {
        (ErrorSymbol, vSymbol, rSymbol)
      }
      // If output slices are required, construct them
      if (st != StorageTypeWire) {
        val StorageTypeSlices(slices) = st
        // TODO: mark inline
        val eName = entityName + "__oslice__" + pName
        val sliceEntity: Entity = SliceFactory(slices, eName, loc, kind)
        val Sym(entitySymbol) = sliceEntity.ref
        val instanceSymbol = {
          val iName = "oslice__" + pName
          cc.newTermSymbol(iName, loc, entitySymbol.denot.kind)
        }
        fcrSMap(symbol) = (sliceEntity, instanceSymbol)
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
      // Add them to the map
      fcaMap(symbol) = if (kind != TypeVoid) {
        (pSymbol, vSymbol, aSymbol)
      } else {
        (ErrorSymbol, vSymbol, aSymbol)
      }
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
      // Add them to the map
      fcaMap(symbol) = if (kind != TypeVoid) {
        (pSymbol, vSymbol, aSymbol)
      } else {
        (ErrorSymbol, vSymbol, aSymbol)
      }
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
    // Nodes with children that have been rewritten need their types assigned
    if (!tree.hasTpe) {
      TypeAssigner(tree)
    }

    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      // We used the Error symbol for void port payloads, now replace
      // the corresponding statement with an empty statement
      case StmtExpr(ExprRef(Sym(ErrorSymbol))) => StmtBlock(Nil)

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "write"), args)) => {
        if (fcnSet contains symbol) {
          StmtAssign(ref, args.head)
        } else if (fcvMap contains symbol) {
          val (pSymbol, vSymbol) = fcvMap(symbol)
          val vAssign = assignTrue(ExprRef(Sym(vSymbol)))
          if (pSymbol != ErrorSymbol) {
            val pAssign = StmtAssign(ExprRef(Sym(pSymbol)), args.head)
            StmtBlock(List(pAssign, vAssign))
          } else {
            vAssign
          }
        } else if (fcrSMap contains symbol) {
          val pSymbol = fcrOMap(symbol)._1
          val (_, iSymbol) = fcrSMap(symbol)
          val iRef = ExprRef(Sym(iSymbol))
          val vAssign = assignTrue(iRef select "u_valid")
          val rStall = stall(~(iRef select "u_ready"))
          if (pSymbol != ErrorSymbol) {
            val pAssign = StmtAssign(iRef select "u_payload", args.head)
            StmtBlock(List(pAssign, vAssign, rStall))
          } else {
            StmtBlock(List(vAssign, rStall))
          }
        } else if (fcaMap contains symbol) {
          val (pSymbol, vSymbol, aSymbol) = fcaMap(symbol)
          val vAssign = assignTrue(ExprRef(Sym(vSymbol)))
          val aStall = stall(!ExprRef(Sym(aSymbol)))
          if (pSymbol != ErrorSymbol) {
            val pAssign = StmtAssign(ExprRef(Sym(pSymbol)), args.head)
            StmtBlock(List(pAssign, vAssign, aStall))
          } else {
            StmtBlock(List(vAssign, aStall))
          }
        } else {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "wait"), args)) => {
        if (fcvMap contains symbol) {
          val (_, vSymbol) = fcvMap(symbol)
          stall(!ExprRef(Sym(vSymbol)))
        } else if (fcrIMap contains symbol) {
          val (_, vSymbol, _) = fcrIMap(symbol)
          stall(!ExprRef(Sym(vSymbol)))
        } else {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "flush"), args)) => {
        if (fcvMap contains symbol) {
          val (_, vSymbol) = fcvMap(symbol)
          stall(ExprRef(Sym(vSymbol)))
        } else if (fcrSMap contains symbol) {
          val (_, iSymbol) = fcrSMap(symbol)
          val iRef = ExprRef(Sym(iSymbol))
          stall(~(iRef select "empty"))
        } else {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ref @ ExprRef(Sym(symbol: TermSymbol)), "read"), Nil) => {
        if (fcnSet contains symbol) {
          ref
        } else if (fcvMap contains symbol) {
          val (pSymbol, vSymbol) = fcvMap(symbol)
          extraStmts.top append stall(!ExprRef(Sym(vSymbol)))
          ExprRef(Sym(pSymbol))
        } else if (fcrIMap contains symbol) {
          val (pSymbol, vSymbol, rSymbol) = fcrIMap(symbol)
          extraStmts.top append stall(!ExprRef(Sym(vSymbol)))
          extraStmts.top append assignTrue(ExprRef(Sym(rSymbol)))
          ExprRef(Sym(pSymbol))
        } else if (fcaMap contains symbol) {
          val (pSymbol, vSymbol, aSymbol) = fcaMap(symbol)
          extraStmts.top append stall(!ExprRef(Sym(vSymbol)))
          extraStmts.top append assignTrue(ExprRef(Sym(aSymbol)))
          ExprRef(Sym(pSymbol))
        } else {
          tree
        }
      }

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "valid"), Nil) => {
        if (fcvMap contains symbol) {
          ExprRef(Sym(fcvMap(symbol)._2))
        } else if (fcrIMap contains symbol) {
          ExprRef(Sym(fcrIMap(symbol)._2))
        } else if (fcrOMap contains symbol) {
          ExprRef(Sym(fcrOMap(symbol)._2))
        } else {
          tree
        }
      }

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "empty"), Nil) => {
        fcrSMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "empty"
        } getOrElse {
          tree
        }
      }

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "full"), Nil) => {
        fcrSMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "full"
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Replace declarations with declarations of the expanded symbols
      //////////////////////////////////////////////////////////////////////////

      case Decl(Sym(symbol: TermSymbol), _, _) => {
        if (fcvMap contains symbol) {
          val (pSymbol, vSymbol) = fcvMap(symbol)
          val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
          if (pSymbol != ErrorSymbol) {
            val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
            Thicket(List(pDecl, vDecl))
          } else {
            vDecl
          }
        } else if (fcrIMap contains symbol) {
          val (pSymbol, vSymbol, rSymbol) = fcrIMap(symbol)
          val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
          val rDecl = Decl(Sym(rSymbol), rSymbol.denot.kind, None)
          if (pSymbol != ErrorSymbol) {
            val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
            Thicket(List(pDecl, vDecl, rDecl))
          } else {
            Thicket(List(vDecl, rDecl))
          }
        } else if (fcrOMap contains symbol) {
          val (pSymbol, vSymbol, rSymbol) = fcrOMap(symbol)
          val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
          val rDecl = Decl(Sym(rSymbol), rSymbol.denot.kind, None)
          if (pSymbol != ErrorSymbol) {
            val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
            Thicket(List(pDecl, vDecl, rDecl))
          } else {
            Thicket(List(vDecl, rDecl))
          }
        } else if (fcaMap contains symbol) {
          val (pSymbol, vSymbol, aSymbol) = fcaMap(symbol)
          val vDecl = Decl(Sym(vSymbol), vSymbol.denot.kind, None)
          val aDecl = Decl(Sym(aSymbol), aSymbol.denot.kind, None)
          if (pSymbol != ErrorSymbol) {
            val pDecl = Decl(Sym(pSymbol), pSymbol.denot.kind, None)
            Thicket(List(pDecl, vDecl, aDecl))
          } else {
            Thicket(List(vDecl, aDecl))
          }
        } else {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add storage slice entities
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity if fcrSMap.nonEmpty => {
        val instances = for ((entity, instance) <- fcrSMap.values) yield {
          Instance(Sym(instance), entity.ref, Nil, Nil)
        }

        val connects = fcrSMap.iterator flatMap {
          case (symbol, (_, iSymbol)) =>
            val (pSymbol, vSymbol, rSymbol) = fcrOMap(symbol)
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

        val entities = fcrSMap.values map { _._1 }
        val thisEntity = entity.copy(
          instances = instances.toList ::: entity.instances,
          connects = connects.toList ::: entity.connects
        ) withVariant entity.variant
        Thicket(thisEntity :: entities.toList)
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
    if (result2 != tree) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "Lost node.tpe", node.toString)
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeOut] => {
        cc.ice(node, s"Output port .${sel} remains")
      }
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeIn] => {
        cc.ice(node, s"Input port .${sel} remains")
      }
      case node @ Decl(_, TypeOut(_, fc, _), _) if fc != FlowControlTypeNone => {
        cc.ice(node, s"Port with flow control remains")
      }
      case node @ Decl(_, TypeOut(_, _, _: StorageTypeSlices), _) => {
        cc.ice(node, s"Port with output slices remains")
      }
    }
  }

}
