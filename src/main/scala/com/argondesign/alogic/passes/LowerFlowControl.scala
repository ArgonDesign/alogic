////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
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
// - Lower output storage slices into output slice instances
// - Update Instances/Connects
// - Replace naked port references
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.SyncRegFactory
import com.argondesign.alogic.core.SyncSliceFactory
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerFlowControlA(
    globalReplacements: mutable.Map[Symbol, Symbol],
    portMap: mutable.Map[Symbol, List[Option[Symbol]]]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private val sep = cc.sep

  // Map from output port symbol to output storage entity, instance symbol,
  // and a boolean that is true if the storage is multiple output slices
  private val oStorage = mutable.Map[Symbol, ((DeclEntity, DefnEntity), Symbol, Boolean)]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()

  private[this] val fctn = FlowControlTypeNone
  private[this] val stw = StorageTypeWire

  // Some statements can be completely removed, this flag marks them
  private[this] var removeStmt = false

  // New entities created in this pass
  private[this] val extraEntities = new ListBuffer[(DeclEntity, DefnEntity)]

  private var first = true

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeType(_: TypeEntity) =>
      first tap { _ =>
        first = false
      }
    case _ => false
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {

      case DeclEntity(symbol, _) =>
        val eSymbol = orig(symbol)
        globalReplacements(eSymbol) = symbol
        symbol.attr.highLevelKind set eSymbol.kind.asType.kind.asEntity

      case Decl(symbol) =>
        symbol.kind match {
          ////////////////////////////////////////////////////////////////////////////
          // FlowControlTypeNone
          ////////////////////////////////////////////////////////////////////////////

          case TypeIn(_, FlowControlTypeNone) =>
            symbol.attr.payloadOfPort set symbol
            portMap(symbol) = List(Some(symbol), None, None, None)

          case TypeOut(_, FlowControlTypeNone, _) =>
            symbol.attr.payloadOfPort set symbol
            portMap(symbol) = List(Some(symbol), None, None, None)

          ////////////////////////////////////////////////////////////////////////////
          // FlowControlTypeValid
          ////////////////////////////////////////////////////////////////////////////

          case TypeIn(kind, FlowControlTypeValid) =>
            // Allocate payload and valid signals
            val loc = tree.loc
            val pName = symbol.name
            val vName = pName + sep + "valid"
            lazy val pSymbol = cc.newSymbol(pName, loc) tap { s =>
              s.kind = TypeIn(kind, fctn)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = cc.newSymbol(vName, loc) tap { s =>
              s.kind = TypeIn(TypeUInt(1), fctn)
              s.attr.validOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) (Some(pSymbol), vSymbol) else (None, vSymbol)
            portMap(symbol) = List(newSymbols._1, Some(newSymbols._2), None, None)

          case TypeOut(kind, FlowControlTypeValid, st) =>
            // Allocate payload and valid signals
            val loc = tree.loc
            val pName = symbol.name
            val vName = pName + sep + "valid"
            lazy val pSymbol = cc.newSymbol(pName, loc) tap { s =>
              s.kind = TypeOut(kind, fctn, stw)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = cc.newSymbol(vName, loc) tap { s =>
              s.kind = TypeOut(TypeUInt(1), fctn, stw)
              s.attr.validOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) (Some(pSymbol), vSymbol) else (None, vSymbol)
            portMap(symbol) = List(newSymbols._1, Some(newSymbols._2), None, None)
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
                cc.newSymbol(iName, loc) tap { _.kind = sregEntity._1.symbol.kind.asType.kind }
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
            lazy val pSymbol = cc.newSymbol(pName, loc) tap { s =>
              s.kind = TypeIn(kind, fctn)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = cc.newSymbol(vName, loc) tap { s =>
              s.kind = TypeIn(TypeUInt(1), fctn)
              s.attr.validOfPort set symbol
            }
            val rSymbol = cc.newSymbol(rName, loc) tap { s =>
              s.kind = TypeOut(TypeUInt(1), fctn, stw)
              s.attr.readyOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) {
              (Some(pSymbol), vSymbol, rSymbol)
            } else {
              (None, vSymbol, rSymbol)
            }
            // Set attributes
            portMap(symbol) = List(newSymbols._1, Some(newSymbols._2), Some(newSymbols._3), None)
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
            lazy val pSymbol = cc.newSymbol(pName, loc) tap { s =>
              s.kind = TypeOut(kind, fctn, stw)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = cc.newSymbol(vName, loc) tap { s =>
              s.kind = TypeOut(TypeUInt(1), fctn, stw)
              s.attr.validOfPort set symbol
            }
            val rSymbol = cc.newSymbol(rName, loc) tap { s =>
              s.kind = TypeIn(TypeUInt(1), fctn)
              s.attr.readyOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) {
              (Some(pSymbol), vSymbol, rSymbol)
            } else {
              (None, vSymbol, rSymbol)
            }
            // Set attributes
            portMap(symbol) = List(newSymbols._1, Some(newSymbols._2), Some(newSymbols._3), None)
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
                  cc.newSymbol(iName, loc) tap {
                    _.kind = sliceEntities.head._1.symbol.kind.asType.kind
                  }
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
            lazy val pSymbol = cc.newSymbol(pName, loc) tap { _.kind = TypeIn(kind, fctn) }
            val vSymbol = cc.newSymbol(vName, loc) tap { _.kind = TypeIn(TypeUInt(1), fctn) }
            val aSymbol = cc.newSymbol(aName, loc) tap { _.kind = TypeOut(TypeUInt(1), fctn, stw) }
            val newSymbols = if (kind != TypeVoid) {
              (Some(pSymbol), vSymbol, aSymbol)
            } else {
              (None, vSymbol, aSymbol)
            }
            // Set attributes
            portMap(symbol) = List(newSymbols._1, Some(newSymbols._2), None, Some(newSymbols._3))
            aSymbol.attr.default set (ExprInt(false, 1, 0) withLoc loc)

          case TypeOut(kind, FlowControlTypeAccept, _) =>
            // Allocate payload, valid and ready signals
            val loc = tree.loc
            val pName = symbol.name
            val vName = pName + sep + "valid"
            val aName = pName + sep + "accept"
            lazy val pSymbol = cc.newSymbol(pName, loc) tap { _.kind = TypeOut(kind, fctn, stw) }
            val vSymbol = cc.newSymbol(vName, loc) tap { _.kind = TypeOut(TypeUInt(1), fctn, stw) }
            val aSymbol = cc.newSymbol(aName, loc) tap { _.kind = TypeIn(TypeUInt(1), fctn) }
            val newSymbols = if (kind != TypeVoid) {
              (Some(pSymbol), vSymbol, aSymbol)
            } else {
              (None, vSymbol, aSymbol)
            }
            // Set attributes
            portMap(symbol) = List(newSymbols._1, Some(newSymbols._2), None, Some(newSymbols._3))
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

      case StmtExpr(ExprCall(ExprSelect(ExprSym(symbol), _, _), _)) =>
        extraStmts.push(ListBuffer())

        // We can remove 'port._();' statements altogether
        removeStmt = portMap contains symbol

      case _: Stmt =>
        // Whenever we enter a new statement, add a new buffer to
        // store potential extra statements
        extraStmts.push(ListBuffer())

      case _ =>
    }
    None
  }

  private[this] def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Drop removed statements
      //////////////////////////////////////////////////////////////////////////

      case _: Stmt if removeStmt =>
        Stump tap { _ =>
          removeStmt = false
        }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ref @ ExprSym(symbol), "read", _), Nil) =>
        portMap.get(symbol) map {
          case List(Some(`symbol`), None, None, None) => // No flow control
            ref
          case List(pSymbolOpt, Some(vSymbol), None, None) => // valid
            extraStmts.top append StmtStall(ExprSym(vSymbol))
            pSymbolOpt map ExprSym getOrElse tree
          case List(pSymbolOpt, Some(vSymbol), Some(rSymbol), None) => // ready
            extraStmts.top append assignTrue(ExprSym(rSymbol))
            extraStmts.top append StmtStall(ExprSym(vSymbol))
            pSymbolOpt map ExprSym getOrElse tree
          case List(pSymbolOpt, Some(vSymbol), None, Some(aSymbol)) => // accept
            extraStmts.top append assignTrue(ExprSym(aSymbol))
            extraStmts.top append StmtStall(ExprSym(vSymbol))
            pSymbolOpt map ExprSym getOrElse tree
          case _ => unreachable
        } getOrElse tree

      case ExprCall(ExprSelect(ref @ ExprSym(symbol), "write", _), args) =>
        lazy val arg = args.head.asInstanceOf[ArgP].expr
        oStorage.get(symbol) match {
          case Some((_, iSymbol, _)) =>
            val iRef = ExprSym(iSymbol)
            portMap.get(symbol) map {
              case List(pSymbolOpt, Some(_), None, None) => // valid
                pSymbolOpt foreach { _ =>
                  extraStmts.top append StmtAssign(iRef select "ip", arg)
                }
                extraStmts.top append assignTrue(iRef select s"ip${sep}valid")
              case List(pSymbolOpt, Some(_), Some(_), None) => // ready
                pSymbolOpt foreach { _ =>
                  extraStmts.top append StmtAssign(iRef select "ip", arg)
                }
                extraStmts.top append assignTrue(iRef select s"ip${sep}valid")
                extraStmts.top append StmtStall(iRef select s"ip${sep}ready")
              case _ => unreachable
            }
          case None =>
            portMap.get(symbol) foreach {
              case List(Some(`symbol`), None, None, None) => // No flow control
                extraStmts.top append StmtAssign(ref, arg)
              case List(pSymbolOpt, Some(vSymbol), None, None) => // valid
                pSymbolOpt foreach { pSymbol =>
                  extraStmts.top append StmtAssign(ExprSym(pSymbol), arg)
                }
                extraStmts.top append assignTrue(ExprSym(vSymbol))
              case List(pSymbolOpt, Some(vSymbol), None, Some(aSymbol)) => // accept
                pSymbolOpt foreach { pSymbol =>
                  extraStmts.top append StmtAssign(ExprSym(pSymbol), arg)
                }
                extraStmts.top append assignTrue(ExprSym(vSymbol))
                extraStmts.top append StmtStall(ExprSym(aSymbol))
              case _ => unreachable
            }
        }
        tree

      case ExprCall(ExprSelect(ExprSym(symbol), "wait", _), _) =>
        assert(removeStmt)
        portMap.get(symbol) foreach {
          case List(_, Some(vSymbol), Some(_), _) =>
            extraStmts.top append StmtStall(ExprSym(vSymbol))
          case _ => unreachable
        }
        tree

      case ExprCall(ExprSelect(ExprSym(symbol), "flush", _), _) =>
        assert(removeStmt)
        oStorage.get(symbol) foreach {
          case (_, iSymbol, false) =>
            extraStmts.top append StmtStall(ExprSym(iSymbol) select "space")
          case (_, iSymbol, true) =>
            extraStmts.top append StmtStall(ExprSym(iSymbol) select "space" unary "&")
        }
        tree

      case ExprSelect(ExprSym(symbol), "valid", _) =>
        portMap.get(symbol) map {
          case List(_, Some(vSymbol), _, None) => ExprSym(vSymbol)
          case _                               => unreachable
        } getOrElse tree

      case ExprSelect(ExprSym(symbol), "space", _) =>
        oStorage.get(symbol) map {
          case (_, iSymbol, _) => ExprSym(iSymbol) select "space"
        } getOrElse {
          tree
        }

      case ExprSelect(ExprSym(symbol), "empty", _) =>
        oStorage.get(symbol) map {
          case (_, iSymbol, false) => ExprSym(iSymbol) select "space"
          case (_, iSymbol, true)  => ExprSym(iSymbol) select "space" unary "&"
        } getOrElse {
          tree
        }

      case ExprSelect(ExprSym(symbol), "full", _) =>
        oStorage.get(symbol) map {
          case (_, iSymbol, false) => ~(ExprSym(iSymbol) select "space")
          case (_, iSymbol, true)  => ~(ExprSym(iSymbol) select "space" unary "|")
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Add decl of the expanded symbols and storage component
      //////////////////////////////////////////////////////////////////////////

      case Decl(symbol) =>
        portMap.get(symbol) map { loweredSymbolOpts =>
          val portDecls = loweredSymbolOpts.iterator.flatten map {
            case `symbol` => tree
            case nSymbol  => nSymbol.mkDecl
          }
          val storageDecl = oStorage.get(symbol).iterator map {
            case (_, sSymbol, _) => sSymbol.mkDecl
          }
          Thicket(List.from(portDecls ++ storageDecl))
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Add defn of the expanded symbols and storage component and wire them up
      //////////////////////////////////////////////////////////////////////////

      case EntDefn(Defn(symbol)) =>
        // Note: Output port defaults, including for flow control signals will
        // be set in the DefaultAssignments/TieOffInputs pass
        portMap.get(symbol) map { loweredSymbolOpts =>
          val portDefns = loweredSymbolOpts.iterator.flatten map {
            case `symbol` => tree
            case nSymbol  => EntDefn(nSymbol.mkDefn)
          }
          val storageDefnAndConnects = oStorage.get(symbol).iterator flatMap {
            case (_, sSymbol, _) =>
              Iterator.single(EntDefn(sSymbol.mkDefn)) ++ {
                val iRef = ExprSym(sSymbol)
                loweredSymbolOpts match {
                  case List(pSymbolOpt, Some(vSymbol), rSymbolOpt, None) =>
                    (pSymbolOpt.iterator map { pSymbol =>
                      EntConnect(iRef select "op", List(ExprSym(pSymbol)))
                    }) ++ Iterator.single {
                      EntConnect(iRef select s"op${sep}valid", List(ExprSym(vSymbol)))
                    } ++ (rSymbolOpt.iterator map { rSymbol =>
                      EntConnect(ExprSym(rSymbol), List(iRef select s"op${sep}ready"))
                    })
                  case _ => unreachable
                }
              }
          }
          Thicket(List.from(portDefns ++ storageDefnAndConnects))
        } getOrElse tree

      //
      case _ => tree
    }

    // Emit any extra statement with this statement
    val result2 = result match {
      case Stump =>
        val extra = extraStmts.pop()
        if (extra.isEmpty) Stump else Thicket(extra.toList)
      case stmt: Stmt =>
        val extra = extraStmts.pop()
        if (extra.isEmpty) stmt else Thicket((extra append stmt).toList)
      case _ => result
    }

    // If we did modify the node, regularize it
    if (result2 ne tree) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finish(tree: Tree): Tree = tree match {
    case _: DeclEntity =>
      Thicket(tree :: (extraEntities.toList map { _._1 }))
    case _: DefnEntity =>
      Thicket(tree :: (extraEntities.toList map { _._2 }))
    case _ => unreachable
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    tree visit {
      case node @ ExprCall(ExprSelect(ref, sel, _), _) if ref.tpe.isOut =>
        cc.ice(node, s"Output port .$sel() remains")
      case node @ ExprCall(ExprSelect(ref, sel, _), _) if ref.tpe.isIn =>
        cc.ice(node, s"Input port .$sel() remains")
      case node @ DeclIn(_, _, fc) if fc != FlowControlTypeNone =>
        cc.ice(node, "Input port with flow control remains")
      case node @ DeclOut(_, _, fc, _) if fc != FlowControlTypeNone =>
        cc.ice(node, "Output port with flow control remains")
      case node @ DeclOut(_, _, _: StorageTypeSlices, _) =>
        cc.ice(node, "Output port with slices remains")
    }
  }

}

final class LowerFlowControlB(
    globalReplacements: collection.Map[Symbol, Symbol],
    portMaps: collection.Map[Symbol, collection.Map[Symbol, List[Option[Symbol]]]]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  //////////////////////////////////////////////////////////////////////////////
  // Extractor mechanism used to convert references to lowered versions
  //////////////////////////////////////////////////////////////////////////////

  type Extractor = List[Option[Symbol]] => Option[Symbol]

  // Given a symbol, return the corresponding payload symbol, if any
  private[this] val payloadSymbol: Extractor = _.head

  // Given a symbol, return the corresponding valid symbol, if any
  private[this] val validSymbol: Extractor = _(1)

  // Given a symbol, return the corresponding ready/accept symbol, if any
  private[this] val backSymbol: Extractor = { ls =>
    ls(2) orElse ls(3)
  }

  // Extractor function to use during current tree walk
  private var extractor: Extractor = payloadSymbol

  // Abort extraction due to missing lowered symbol
  private var abortExtraction = false

  // Walk tree, replacing references with the extracted symbols
  private def extract(expr: Expr, extractor: Extractor): Option[Expr] = {
    assert(this.extractor eq payloadSymbol)
    assert(!abortExtraction)
    this.extractor = extractor
    val result = walk(expr).asInstanceOf[Expr]
    if (abortExtraction) None else Some(result)
  } tap { _ =>
    this.extractor = payloadSymbol
    abortExtraction = false
  }

  //////////////////////////////////////////////////////////////////////////////
  // The tree transform
  //////////////////////////////////////////////////////////////////////////////

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  // This contains all entities replaced in LowerFlowControlA
  private val replacedInPassA = Set.from(globalReplacements.valuesIterator)

  override def skip(tree: Tree): Boolean = tree match {
    case DeclEntity(symbol, _)    => !replacedInPassA(symbol)
    case DefnEntity(symbol, _, _) => !replacedInPassA(symbol)
    case _                        => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Select on instance
    case ExprSelect(expr @ ExprSym(iSymbol), sel, _) if iSymbol.kind.isEntity =>
      Some {
        val kind = iSymbol.kind.asEntity
        globalReplacements.get(kind.symbol) match {
          // The entity was not replaced (it is a slice/reg added in LowerFlowControlA)
          case None => tree
          // Replace with reference to extracted signal of replaced instance
          case Some(rSymbol) =>
            val pSymbol = kind(sel).get
            val portMap = portMaps(rSymbol)
            extractor(portMap(pSymbol)) match {
              // No extracted signal, abort extraction
              case None => abortExtraction = true; tree
              // Replace with reference to extracted port
              case Some(symbol) =>
                walk(expr).asInstanceOf[Expr] select symbol.name tap { _ =>
                  abortExtraction = false
                }
            }
        }
      }

    // Connection
    case EntConnect(lhs, List(rhs)) =>
      Some {
        // Expand inter-entity connections
        val pLhs = extract(lhs, payloadSymbol)
        val pRhs = extract(rhs, payloadSymbol)

        val vLhs = extract(lhs, validSymbol)
        val vRhs = extract(rhs, validSymbol)

        val bLhs = extract(lhs, backSymbol)
        val bRhs = extract(rhs, backSymbol)

        val pConn = pLhs flatMap { lhs =>
          pRhs map { rhs =>
            EntConnect(lhs, List(rhs)) regularize tree.loc
          }
        }
        val vConn = vLhs flatMap { lhs =>
          vRhs map { rhs =>
            EntConnect(lhs, List(rhs)) regularize tree.loc
          }
        }
        val bConn = bLhs flatMap { lhs =>
          bRhs map { rhs =>
            EntConnect(rhs, List(lhs)) regularize tree.loc
          }
        }

        Thicket(List.concat(pConn, vConn, bConn))
      }

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Update instance types
    //////////////////////////////////////////////////////////////////////////

    case decl @ DeclInstance(_, ExprSym(eSymbol)) =>
      globalReplacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    //////////////////////////////////////////////////////////////////////////
    // Expressions
    //////////////////////////////////////////////////////////////////////////

    case ExprSym(pSymbol) =>
      // Rewrite references to ports as references to the extracted signal
      portMaps(entitySymbol).get(pSymbol) match {
        // Not a port
        case None =>
          abortExtraction |= extractor ne payloadSymbol
          tree
        // It is a port
        case Some(ls) =>
          extractor(ls) match {
            // No extracted signal, abort extraction
            case None => abortExtraction = true; tree
            // Replace with reference to extracted signal
            case Some(sSymbol) => ExprSym(sSymbol) regularize tree.loc
          }
      }

    //
    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(!abortExtraction)
  }

}

object LowerFlowControl {

  def apply(): Pass[List[(Decl, Defn)], List[(Decl, Defn)]] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()
    val portMaps = TrieMap[Symbol, mutable.Map[Symbol, List[Option[Symbol]]]]()

    new EntityTransformerPass(declFirst = true) {
      val name = "lower-flow-control-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = {
        val portMap = mutable.Map[Symbol, List[Option[Symbol]]]()
        portMaps(symbol) = portMap
        new LowerFlowControlA(globalReplacements, portMap)
      }
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "lower-flow-control-b"

      // Remap the keys to their replacements
      lazy val pMaps: collection.Map[Symbol, collection.Map[Symbol, List[Option[Symbol]]]] =
        portMaps map { case (k, v) => (globalReplacements(k), v) }

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerFlowControlB(globalReplacements, pMaps)
    }
  }

}
