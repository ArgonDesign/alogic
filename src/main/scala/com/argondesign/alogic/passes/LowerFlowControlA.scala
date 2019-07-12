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
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.SyncRegFactory
import com.argondesign.alogic.core.SyncSliceFactory
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerFlowControlA(implicit cc: CompilerContext) extends TreeTransformer {

  // TODO: rework without using ErrorSymbol, use removeStmt instead

  private val sep = cc.sep

  // Map from output port symbol to output storage entity, instance symbol,
  // and a boolean that is true if the storage is multiple output slices
  private val oStorage = mutable.Map[TermSymbol, (Entity, TermSymbol, Boolean)]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()

  private[this] def boolType(loc: Loc): TypeUInt = {
    val one = Expr(1) withLoc loc
    TypeAssigner(one)
    TypeUInt(one)
  }

  private[this] val fctn = FlowControlTypeNone
  private[this] val stw = StorageTypeWire

  // Some statements can be completely removed, this flag marks them
  private[this] var removeStmt = false

  // New entities created in this pass
  private[this] val extraEntities = new ListBuffer[Entity]

  override def enter(tree: Tree): Unit = tree match {

    case Decl(symbol, _) =>
      symbol.kind match {
        ////////////////////////////////////////////////////////////////////////////
        // FlowControlTypeNone
        ////////////////////////////////////////////////////////////////////////////

        case TypeIn(_, FlowControlTypeNone) =>
          symbol.attr.fcn set true

        case TypeOut(_, FlowControlTypeNone, _) =>
          symbol.attr.fcn set true

        ////////////////////////////////////////////////////////////////////////////
        // FlowControlTypeValid
        ////////////////////////////////////////////////////////////////////////////

        case TypeIn(kind, FlowControlTypeValid) =>
          // Allocate payload and valid signals
          val loc = tree.loc
          val pName = symbol.name
          val vName = pName + sep + "valid"
          lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeIn(kind, fctn))
          val vSymbol = cc.newTermSymbol(vName, loc, TypeIn(boolType(loc), fctn))
          val newSymbols = if (kind != TypeVoid) (pSymbol, vSymbol) else (ErrorSymbol, vSymbol)
          // Set attributes
          symbol.attr.fcv set newSymbols
          symbol.attr.expandedPort set true

        case TypeOut(kind, FlowControlTypeValid, st) =>
          // Allocate payload and valid signals
          val loc = tree.loc
          val pName = symbol.name
          val vName = pName + sep + "valid"
          lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fctn, stw))
          val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fctn, stw))
          val newSymbols = if (kind != TypeVoid) (pSymbol, vSymbol) else (ErrorSymbol, vSymbol)
          // Set attributes
          symbol.attr.fcv set newSymbols
          symbol.attr.expandedPort set true
          if (st == StorageTypeWire) {
            vSymbol.attr.default set (ExprInt(false, 1, 0) withLoc loc)
            vSymbol.attr.clearOnStall set true
          } else {
            // If a synchronous output register is required, construct it
            // TODO: mark inline
            val eName = entitySymbol.name + sep + "or" + sep + pName
            val sregEntity = SyncRegFactory(eName, loc, kind)
            extraEntities append sregEntity
            val iSymbol = {
              val iName = "or" + sep + pName
              cc.newTermSymbol(iName, loc, TypeInstance(sregEntity.symbol))
            }
            // Set attributes
            oStorage(symbol) = (sregEntity, iSymbol, false)
            entitySymbol.attr.interconnectClearOnStall.append((iSymbol, s"ip${sep}valid"))
          }

        ////////////////////////////////////////////////////////////////////////////
        // FlowControlTypeReady
        ////////////////////////////////////////////////////////////////////////////

        case TypeIn(kind, FlowControlTypeReady) =>
          // Allocate payload, valid and ready signals
          val loc = tree.loc
          val pName = symbol.name
          val vName = pName + sep + "valid"
          val rName = pName + sep + "ready"
          lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeIn(kind, fctn))
          val vSymbol = cc.newTermSymbol(vName, loc, TypeIn(boolType(loc), fctn))
          val rSymbol = cc.newTermSymbol(rName, loc, TypeOut(boolType(loc), fctn, stw))
          val newSymbols = if (kind != TypeVoid) {
            (pSymbol, vSymbol, rSymbol)
          } else {
            (ErrorSymbol, vSymbol, rSymbol)
          }
          // Set attributes
          symbol.attr.fcr set newSymbols
          symbol.attr.expandedPort set true
          rSymbol.attr.default set (ExprInt(false, 1, 0) withLoc loc)
          rSymbol.attr.clearOnStall set true
          rSymbol.attr.dontCareUnless set vSymbol
          vSymbol.attr.dontCareUnless set rSymbol

        case TypeOut(kind, FlowControlTypeReady, st) =>
          // Allocate payload, valid and ready signals
          val loc = tree.loc
          val pName = symbol.name
          val vName = pName + sep + "valid"
          val rName = pName + sep + "ready"
          lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fctn, stw))
          val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fctn, stw))
          val rSymbol = cc.newTermSymbol(rName, loc, TypeIn(boolType(loc), fctn))
          val newSymbols = if (kind != TypeVoid) {
            (pSymbol, vSymbol, rSymbol)
          } else {
            (ErrorSymbol, vSymbol, rSymbol)
          }
          // Set attributes
          symbol.attr.fcr set newSymbols
          symbol.attr.expandedPort set true
          rSymbol.attr.dontCareUnless set vSymbol
          vSymbol.attr.dontCareUnless set rSymbol
          // If output slices are required, construct them
          st match {
            case StorageTypeWire           =>
            case StorageTypeSlices(slices) =>
              // TODO: mark inline
              val ePrefix = entitySymbol.name + sep + pName
              val sliceEntities = SyncSliceFactory(slices, ePrefix, loc, kind)
              extraEntities appendAll sliceEntities
              val iSymbol = {
                val iName = "os" + sep + pName
                cc.newTermSymbol(iName, loc, TypeInstance(sliceEntities.head.symbol))
              }
              // Set attributes
              oStorage(symbol) = (sliceEntities.head, iSymbol, sliceEntities.tail.nonEmpty)
              entitySymbol.attr.interconnectClearOnStall.append((iSymbol, s"ip${sep}valid"))
            case _ => unreachable
          }

        ////////////////////////////////////////////////////////////////////////////
        // FlowControlTypeAccept
        ////////////////////////////////////////////////////////////////////////////

        case TypeIn(kind, FlowControlTypeAccept) =>
          // Allocate payload, valid and accept signals
          val loc = tree.loc
          val pName = symbol.name
          val vName = pName + sep + "valid"
          val aName = pName + sep + "accept"
          lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeIn(kind, fctn))
          val vSymbol = cc.newTermSymbol(vName, loc, TypeIn(boolType(loc), fctn))
          val aSymbol = cc.newTermSymbol(aName, loc, TypeOut(boolType(loc), fctn, stw))
          val newSymbols = if (kind != TypeVoid) {
            (pSymbol, vSymbol, aSymbol)
          } else {
            (ErrorSymbol, vSymbol, aSymbol)
          }
          // Set attributes
          symbol.attr.fca set newSymbols
          symbol.attr.expandedPort set true
          aSymbol.attr.default set (ExprInt(false, 1, 0) withLoc loc)

        case TypeOut(kind, FlowControlTypeAccept, _) =>
          // Allocate payload, valid and ready signals
          val loc = tree.loc
          val pName = symbol.name
          val vName = pName + sep + "valid"
          val aName = pName + sep + "accept"
          lazy val pSymbol = cc.newTermSymbol(pName, loc, TypeOut(kind, fctn, stw))
          val vSymbol = cc.newTermSymbol(vName, loc, TypeOut(boolType(loc), fctn, stw))
          val aSymbol = cc.newTermSymbol(aName, loc, TypeIn(boolType(loc), fctn))
          val newSymbols = if (kind != TypeVoid) {
            (pSymbol, vSymbol, aSymbol)
          } else {
            (ErrorSymbol, vSymbol, aSymbol)
          }
          // Set attributes
          symbol.attr.fca set newSymbols
          symbol.attr.expandedPort set true
          vSymbol.attr.default set (ExprInt(false, 1, 0) withLoc loc)
          vSymbol.attr.clearOnStall set true
          vSymbol.attr.dontCareUnless set aSymbol

        ////////////////////////////////////////////////////////////////////////////
        // Other decls
        ////////////////////////////////////////////////////////////////////////////

        case _ =>
      }

    ////////////////////////////////////////////////////////////////////////////
    // Statements
    ////////////////////////////////////////////////////////////////////////////

    case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "read"), _)) => {
      extraStmts.push(ListBuffer())

      val attr = symbol.attr

      // We can remove 'port.read();' statements altogether
      removeStmt = attr.fcn.isSet || attr.fcv.isSet || attr.fcr.isSet || attr.fca.isSet
    }

    case _: Stmt => {
      // Whenever we enter a new statement, add a new buffer to
      // store potential extra statements
      extraStmts.push(ListBuffer())
    }

    case _ =>
  }

  private[this] def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case _: Stmt if removeStmt => {
        StmtBlock(Nil)
      } tap { _ =>
        removeStmt = false
      }

      // We used the Error symbol for void port payloads, now replace
      // the corresponding statement with an empty statement
      case StmtExpr(ExprRef(ErrorSymbol)) => StmtBlock(Nil)

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(symbol: TermSymbol), "write"), args)) => {
        symbol.attr.fcn.get.map { _ =>
          StmtAssign(ref, args.head)
        } orElse oStorage.get(symbol).map {
          case (_, iSymbol, _) =>
            lazy val iRef = ExprRef(iSymbol)
            lazy val pAssign = StmtAssign(iRef select "ip", args.head)
            lazy val vAssign = assignTrue(iRef select s"ip${sep}valid")
            lazy val rStall = StmtStall(iRef select s"ip${sep}ready")
            symbol.attr.fcv.get.map {
              case (ErrorSymbol, _) => vAssign
              case _                => StmtBlock(List(pAssign, vAssign))
            } orElse symbol.attr.fcr.get.map {
              case (ErrorSymbol, _, _) => StmtBlock(List(vAssign, rStall))
              case _                   => StmtBlock(List(pAssign, vAssign, rStall))
            } getOrElse {
              unreachable
            }
        } orElse symbol.attr.fcv.get.map {
          case (pSymbol, vSymbol) =>
            val vAssign = assignTrue(ExprRef(vSymbol))
            if (pSymbol != ErrorSymbol) {
              val pAssign = StmtAssign(ExprRef(pSymbol), args.head)
              StmtBlock(List(pAssign, vAssign))
            } else {
              vAssign
            }
        } orElse symbol.attr.fca.get.map {
          case (pSymbol, vSymbol, aSymbol) =>
            val vAssign = assignTrue(ExprRef(vSymbol))
            val aStall = StmtStall(ExprRef(aSymbol))
            if (pSymbol != ErrorSymbol) {
              val pAssign = StmtAssign(ExprRef(pSymbol), args.head)
              StmtBlock(List(pAssign, vAssign, aStall))
            } else {
              StmtBlock(List(vAssign, aStall))
            }
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(symbol: TermSymbol), "wait"), args)) => {
        symbol.attr.fcr.get.map {
          case (_, vSymbol, _) => StmtStall(ExprRef(vSymbol))
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ref @ ExprRef(symbol: TermSymbol), "flush"), args)) => {
        oStorage.get(symbol).map {
          case (_, iSymbol, false) => StmtStall(ExprRef(iSymbol) select "space")
          case (_, iSymbol, true)  => StmtStall(ExprRef(iSymbol) select "space" unary "&")
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ref @ ExprRef(symbol: TermSymbol), "read"), Nil) => {
        symbol.attr.fcn.get.map { _ =>
          ref
        } orElse symbol.attr.fcv.get.map {
          case (pSymbol, vSymbol) =>
            extraStmts.top append StmtStall(ExprRef(vSymbol))
            ExprRef(pSymbol)
        } orElse symbol.attr.fcr.get.map {
          case (pSymbol, vSymbol, rSymbol) =>
            extraStmts.top append assignTrue(ExprRef(rSymbol))
            extraStmts.top append StmtStall(ExprRef(vSymbol))
            ExprRef(pSymbol)
        } orElse symbol.attr.fca.get.map {
          case (pSymbol, vSymbol, aSymbol) =>
            extraStmts.top append assignTrue(ExprRef(aSymbol))
            extraStmts.top append StmtStall(ExprRef(vSymbol))
            ExprRef(pSymbol)
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "valid") => {
        symbol.attr.fcv.get.map {
          case (_, vSymbol) => ExprRef(vSymbol)
        } orElse symbol.attr.fcr.get.map {
          case (_, vSymbol, _) => ExprRef(vSymbol)
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "space") => {
        oStorage.get(symbol) map {
          case (_, iSymbol, _) => ExprRef(iSymbol) select "space"
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "empty") => {
        oStorage.get(symbol) map {
          case (_, iSymbol, false) => ExprRef(iSymbol) select "space"
          case (_, iSymbol, true)  => ExprRef(iSymbol) select "space" unary "&"
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "full") => {
        oStorage.get(symbol) map {
          case (_, iSymbol, false) => ~(ExprRef(iSymbol) select "space")
          case (_, iSymbol, true)  => ~(ExprRef(iSymbol) select "space" unary "|")
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add declarations of the expanded symbols
      //////////////////////////////////////////////////////////////////////////

      case decl @ EntDecl(Decl(symbol, _)) => {
        // Note: We also leave the declaration of the original symbol, as
        // Connect instances have not been rewritten yet. These will be fixed
        // up in a later pass as they required all entities to have been
        // converted before we can type port references
        symbol.attr.fcv.get.map {
          case (pSym, vSymbol) =>
            val vDecl = EntDecl(Decl(vSymbol, None))
            val newDecls = pSym match {
              case ErrorSymbol => List(vDecl)
              case pSymbol: TermSymbol => {
                val pDecl = EntDecl(Decl(pSymbol, None))
                List(pDecl, vDecl)
              }
              case _ => unreachable
            }
            Thicket(decl :: newDecls)
        } orElse symbol.attr.fcr.get.map {
          case (pSym, vSymbol, rSymbol) =>
            val vDecl = EntDecl(Decl(vSymbol, None))
            val rDecl = EntDecl(Decl(rSymbol, None))
            val newDecls = pSym match {
              case ErrorSymbol => List(vDecl, rDecl)
              case pSymbol: TermSymbol => {
                val pDecl = EntDecl(Decl(pSymbol, None))
                List(pDecl, vDecl, rDecl)
              }
              case _ => unreachable
            }
            Thicket(decl :: newDecls)
        } orElse symbol.attr.fca.get.map {
          case (pSym, vSymbol, aSymbol) =>
            val vDecl = EntDecl(Decl(vSymbol, None))
            val aDecl = EntDecl(Decl(aSymbol, None))
            val newDecls = pSym match {
              case ErrorSymbol => List(vDecl, aDecl)
              case pSymbol: TermSymbol => {
                val pDecl = EntDecl(Decl(pSymbol, None))
                List(pDecl, vDecl, aDecl)
              }
              case _ => unreachable
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
        val ospSymbols = entity.declarations collect {
          case Decl(symbol, _) if oStorage contains symbol => symbol
        }

        val instances = for (symbol <- ospSymbols) yield {
          val (entity, iSymbol, _) = oStorage(symbol)
          EntInstance(Sym(iSymbol), Sym(entity.symbol), Nil, Nil)
        }

        val connects = ospSymbols flatMap { symbol =>
          val iSymbol = oStorage(symbol)._2
          val iRef = ExprRef(iSymbol)

          val (pSymbolOpt, vSymbol, rSymbolOpt) = symbol.attr.fcv.get.map {
            case (ErrorSymbol, vSymbol) => (None, vSymbol, None)
            case (pSymbol, vSymbol)     => (Some(pSymbol), vSymbol, None)
          } orElse symbol.attr.fcr.get.map {
            case (ErrorSymbol, vSymbol, rSymbol) => (None, vSymbol, Some(rSymbol))
            case (pSymbol, vSymbol, rSymbol)     => (Some(pSymbol), vSymbol, Some(rSymbol))
          } getOrElse {
            unreachable
          }

          val pConnOpt = pSymbolOpt map { pSymbol =>
            EntConnect(iRef select "op", List(ExprRef(pSymbol)))
          }
          val vConn = EntConnect(iRef select s"op${sep}valid", List(ExprRef(vSymbol)))
          val rConnOpt = rSymbolOpt map { rSymbol =>
            EntConnect(ExprRef(rSymbol), List(iRef select s"op${sep}ready"))
          }

          pConnOpt.toList ::: vConn :: rConnOpt.toList
        }

        // Remove fcn and oStorage attributes, they are no longer needed
        entity.declarations foreach {
          case Decl(symbol, _) => {
            symbol.attr.fcn.clear()
          }
          case _ => unreachable
        }

        // Note: Output port defaults, including for flow control signals will
        // be set in the OutputDefault pass

        // Update type of entity to include the new ports. We also leave the old
        // un-converted port for now, as Connect instances have not been updated
        // yet.
        val portSymbols = entity.declarations collect {
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeIn]  => symbol
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeOut] => symbol
        }

        entitySymbol.kind match {
          case highLevelKind: TypeEntity =>
            entitySymbol.attr.highLevelKind set highLevelKind
            entitySymbol.kind = highLevelKind.copy(portSymbols = portSymbols)
          case _ => unreachable
        }

        val thisEntity = entity.copy(body = entity.body ::: instances ::: connects)

        Thicket(thisEntity :: extraEntities.toList)
      }

      case _ => tree
    }

    // Emit any extra statement with this statement
    val result2 = result match {
      case StmtBlock(Nil) =>
        val extra = extraStmts.pop()
        if (extra.isEmpty) Thicket(Nil) else StmtBlock(extra.toList)
      case stmt: Stmt =>
        val extra = extraStmts.pop()
        if (extra.isEmpty) stmt else StmtBlock((extra append stmt).toList)
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

object LowerFlowControlA extends TreeTransformerPass {
  val name = "lower-flow-control-a"

  def create(implicit cc: CompilerContext) = new LowerFlowControlA
}
