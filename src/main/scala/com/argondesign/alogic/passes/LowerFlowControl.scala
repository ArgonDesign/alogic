////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// - Convert port flow control to stall statements
// - Split ports with flow control into payload + control flow signals
// - Lower output storage slices into output slice instances
// - Update Instances/Connects
// - Replace naked port references
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.passes.LowerFlowControl.LoweredSymbols
import com.argondesign.alogic.util.unreachable

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerFlowControlA(
    globalReplacements: TrieMap[Symbol, Symbol],
    portMap: mutable.Map[Symbol, LoweredSymbols]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private val sep = cc.sep

  // Map from output port symbol to port instance symbol,
  private val oStorage = mutable.Map[Symbol, Symbol]()

  // Stack of extra statements to emit when finished with a statement
  private val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()

  private val fctn = FlowControlTypeNone
  private val stw = StorageTypeWire

  // Some statements can be completely removed, this flag marks them
  private var removeStmt = false

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
            portMap(symbol) = (Some(symbol), None, None)

          case TypeOut(_, FlowControlTypeNone, _) =>
            symbol.attr.payloadOfPort set symbol
            portMap(symbol) = (Some(symbol), None, None)

          ////////////////////////////////////////////////////////////////////////////
          // FlowControlTypeValid
          ////////////////////////////////////////////////////////////////////////////

          case TypeIn(kind, FlowControlTypeValid) =>
            // Allocate payload and valid signals
            val loc = tree.loc
            val pName = symbol.name
            val vName = pName + sep + "valid"
            lazy val pSymbol = Symbol(pName, loc) tap { s =>
              s.kind = TypeIn(kind, fctn)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = Symbol(vName, loc) tap { s =>
              s.kind = TypeIn(TypeUInt(1), fctn)
              s.attr.validOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) (Some(pSymbol), vSymbol) else (None, vSymbol)
            portMap(symbol) = (newSymbols._1, Some(newSymbols._2), None)

          case TypeOut(kind, FlowControlTypeValid, st) =>
            // Allocate payload and valid signals
            val loc = tree.loc
            val pName = symbol.name
            val vName = pName + sep + "valid"
            lazy val pSymbol = Symbol(pName, loc) tap { s =>
              s.kind = TypeOut(kind, fctn, stw)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = Symbol(vName, loc) tap { s =>
              s.kind = TypeOut(TypeUInt(1), fctn, stw)
              s.attr.validOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) (Some(pSymbol), vSymbol) else (None, vSymbol)
            portMap(symbol) = (newSymbols._1, Some(newSymbols._2), None)
            if (st == StorageTypeWire) {
              vSymbol.attr.default set (ExprInt(false, 1, 0) withLoc loc)
              vSymbol.attr.clearOnStall set true
            } else {
              // If a synchronous output register is required, construct it
              // TODO: mark inline
              val sregEntitySymbol = cc.syncRegFactory(kind)
              val iSymbol = {
                val iName = "or" + sep + pName
                Symbol(iName, loc) tap { _.kind = sregEntitySymbol.kind.asType.kind }
              }
              // Set attributes
              oStorage(symbol) = iSymbol
              entitySymbol.attr.interconnectClearOnStall.append((iSymbol, "i_valid"))
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
            lazy val pSymbol = Symbol(pName, loc) tap { s =>
              s.kind = TypeIn(kind, fctn)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = Symbol(vName, loc) tap { s =>
              s.kind = TypeIn(TypeUInt(1), fctn)
              s.attr.validOfPort set symbol
            }
            val rSymbol = Symbol(rName, loc) tap { s =>
              s.kind = TypeOut(TypeUInt(1), fctn, stw)
              s.attr.readyOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) {
              (Some(pSymbol), vSymbol, rSymbol)
            } else {
              (None, vSymbol, rSymbol)
            }
            // Set attributes
            portMap(symbol) = (newSymbols._1, Some(newSymbols._2), Some(newSymbols._3))
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
            lazy val pSymbol = Symbol(pName, loc) tap { s =>
              s.kind = TypeOut(kind, fctn, stw)
              s.attr.payloadOfPort set symbol
            }
            val vSymbol = Symbol(vName, loc) tap { s =>
              s.kind = TypeOut(TypeUInt(1), fctn, stw)
              s.attr.validOfPort set symbol
            }
            val rSymbol = Symbol(rName, loc) tap { s =>
              s.kind = TypeIn(TypeUInt(1), fctn)
              s.attr.readyOfPort set symbol
            }
            val newSymbols = if (kind != TypeVoid) {
              (Some(pSymbol), vSymbol, rSymbol)
            } else {
              (None, vSymbol, rSymbol)
            }
            // Set attributes
            portMap(symbol) = (newSymbols._1, Some(newSymbols._2), Some(newSymbols._3))
            rSymbol.attr.dontCareUnless set vSymbol
            vSymbol.attr.dontCareUnless set rSymbol
            // If output slices are required, construct them
            st match {
              case StorageTypeWire           =>
              case StorageTypeSlices(slices) =>
                // TODO: mark inline
                val sliceEntitiesSymbol = cc.syncSliceFactory(kind, slices)
                val iSymbol = {
                  val iName = "os" + sep + pName
                  Symbol(iName, loc) tap { _.kind = sliceEntitiesSymbol.kind.asType.kind }
                }
                // Set attributes
                oStorage(symbol) = iSymbol
                entitySymbol.attr.interconnectClearOnStall.append((iSymbol, "i_valid"))
              case _ => unreachable
            }

          ////////////////////////////////////////////////////////////////////////////
          // Other decls
          ////////////////////////////////////////////////////////////////////////////

          case _ =>
        }

      ////////////////////////////////////////////////////////////////////////////
      // Statements
      ////////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSel(ExprSym(symbol), _), _)) =>
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

  private def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))

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

      case ExprCall(ExprSel(ref @ ExprSym(symbol), "read"), Nil) =>
        portMap.get(symbol) map {
          case (Some(`symbol`), None, None) => // No flow control
            ref
          case (pSymbolOpt, Some(vSymbol), None) => // valid
            extraStmts.top append StmtWait(ExprSym(vSymbol))
            pSymbolOpt map ExprSym.apply getOrElse tree
          case (pSymbolOpt, Some(vSymbol), Some(rSymbol)) => // ready
            extraStmts.top append assignTrue(ExprSym(rSymbol))
            extraStmts.top append StmtWait(ExprSym(vSymbol))
            pSymbolOpt map ExprSym.apply getOrElse tree
          case _ => unreachable
        } getOrElse tree

      case ExprCall(ExprSel(ref @ ExprSym(symbol), "write"), args) =>
        lazy val arg = args.head.asInstanceOf[ArgP].expr
        oStorage.get(symbol) match {
          case Some(iSymbol) =>
            val iRef = ExprSym(iSymbol)
            portMap.get(symbol) map {
              case (pSymbolOpt, Some(_), None) => // valid
                pSymbolOpt foreach { _ =>
                  extraStmts.top append StmtAssign(iRef sel "i_payload", arg)
                }
                extraStmts.top append assignTrue(iRef sel "i_valid")
              case (pSymbolOpt, Some(_), Some(_)) => // ready
                pSymbolOpt foreach { _ =>
                  extraStmts.top append StmtAssign(iRef sel "i_payload", arg)
                }
                extraStmts.top append assignTrue(iRef sel "i_valid")
                extraStmts.top append StmtWait(iRef sel "i_ready")
              case _ => unreachable
            }
          case None =>
            portMap.get(symbol) foreach {
              case (Some(`symbol`), None, None) => // No flow control
                extraStmts.top append StmtAssign(ref, arg)
              case (pSymbolOpt, Some(vSymbol), None) => // valid
                pSymbolOpt foreach { pSymbol =>
                  extraStmts.top append StmtAssign(ExprSym(pSymbol), arg)
                }
                extraStmts.top append assignTrue(ExprSym(vSymbol))
              case _ => unreachable
            }
        }
        tree

      case ExprSel(ExprSym(symbol), "valid") =>
        portMap.get(symbol) map {
          case (_, Some(vSymbol), _) => ExprSym(vSymbol)
          case _                     => unreachable
        } getOrElse tree

      case ExprSel(ExprSym(symbol), "space") =>
        oStorage.get(symbol) map { iSymbol =>
          ExprSym(iSymbol) sel "space"
        } getOrElse {
          tree
        }

      case ExprSel(ExprSym(symbol), "empty") =>
        oStorage.get(symbol) map { iSymbol =>
          ExprSym(iSymbol) sel "space" unary "&"
        } getOrElse {
          tree
        }

      case ExprSel(ExprSym(symbol), "full") =>
        oStorage.get(symbol) map { iSymbol =>
          ~(ExprSym(iSymbol) sel "space" unary "|")
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Add decl of the expanded symbols and storage component
      //////////////////////////////////////////////////////////////////////////

      case Decl(symbol) =>
        portMap.get(symbol) map { loweredSymbolOpts =>
          val portDecls = loweredSymbolOpts.productIterator flatMap {
            case Some(`symbol`)        => Some(tree)
            case Some(nSymbol: Symbol) => Some(nSymbol.mkDecl)
            case None                  => None
            case _                     => unreachable
          }
          val storageDecl = oStorage.get(symbol).iterator map { sSymbol =>
            sSymbol.mkDecl
          }
          Thicket(List.from(portDecls ++ storageDecl))
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Add defn of the expanded symbols and storage component and wire them up
      //////////////////////////////////////////////////////////////////////////

      case EntSplice(Defn(symbol)) =>
        // Note: Output port defaults, including for flow control signals will
        // be set in the DefaultAssignments/TieOffInputs pass
        portMap.get(symbol) map { loweredSymbolOpts =>
          val portDefns = loweredSymbolOpts.productIterator flatMap {
            case Some(`symbol`)        => Some(tree)
            case Some(nSymbol: Symbol) => Some(EntSplice(nSymbol.mkDefn))
            case None                  => None
            case _                     => unreachable
          }
          val storageDefnAndConnects = oStorage.get(symbol).iterator flatMap { sSymbol =>
            Iterator.single(EntSplice(sSymbol.mkDefn)) ++ {
              val iRef = ExprSym(sSymbol)
              loweredSymbolOpts match {
                case (pSymbolOpt, Some(vSymbol), rSymbolOpt) =>
                  (pSymbolOpt.iterator map { pSymbol =>
                    EntAssign(ExprSym(pSymbol), iRef sel "o_payload")
                  }) ++ Iterator.single {
                    EntAssign(ExprSym(vSymbol), iRef sel "o_valid")
                  } ++ (rSymbolOpt.iterator map { rSymbol =>
                    EntAssign(iRef sel "o_ready", ExprSym(rSymbol))
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
    if ((result2 ne tree) && result2 != Stump) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    // $COVERAGE-OFF$ Debug code
    tree visit {
      case node @ ExprCall(ExprSel(ref, sel), _) if ref.tpe.isOut =>
        throw Ice(node, s"Output port .$sel() remains")
      case node @ ExprCall(ExprSel(ref, sel), _) if ref.tpe.isIn =>
        throw Ice(node, s"Input port .$sel() remains")
      case node @ DeclIn(_, _, fc) if fc != FlowControlTypeNone =>
        throw Ice(node, "Input port with flow control remains")
      case node @ DeclOut(_, _, fc, _) if fc != FlowControlTypeNone =>
        throw Ice(node, "Output port with flow control remains")
      case node @ DeclOut(_, _, _, _: StorageTypeSlices) =>
        throw Ice(node, "Output port with slices remains")
    }
    // $COVERAGE-ON$
  }

}

final class LowerFlowControlB(
    globalReplacements: collection.Map[Symbol, Symbol],
    portMaps: collection.Map[Symbol, collection.Map[Symbol, LoweredSymbols]])
    extends StatefulTreeTransformer {

  //////////////////////////////////////////////////////////////////////////////
  // Extractor mechanism used to convert references to lowered versions
  //////////////////////////////////////////////////////////////////////////////

  type Extractor = LoweredSymbols => Option[Symbol]

  // Given a symbol, return the corresponding payload symbol, if any
  private val payloadSymbol: Extractor = _._1

  // Given a symbol, return the corresponding valid symbol, if any
  private val validSymbol: Extractor = _._2

  // Given a symbol, return the corresponding ready symbol, if any
  private val readySymbol: Extractor = _._3

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

  override def enter(tree: Tree): Option[Tree] = tree match {
    case DeclEntity(symbol, _) if !replacedInPassA(symbol)    => Some(tree)
    case DefnEntity(symbol, _, _) if !replacedInPassA(symbol) => Some(tree)

    // Select on instance
    case ExprSel(expr @ ExprSym(iSymbol), sel) if iSymbol.kind.isEntity =>
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
                walk(expr).asInstanceOf[Expr] sel symbol.name tap { _ =>
                  abortExtraction = false
                }
            }
        }
      }

    // Connection
    case EntAssign(lhs, rhs) =>
      Some {
        // Expand inter-entity connections
        val pRhs = extract(rhs, payloadSymbol)
        val pLhs = extract(lhs, payloadSymbol)

        val vRhs = extract(rhs, validSymbol)
        val vLhs = extract(lhs, validSymbol)

        val bRhs = extract(rhs, readySymbol)
        val bLhs = extract(lhs, readySymbol)

        val pConn = pRhs flatMap { rhs =>
          pLhs map { lhs =>
            EntAssign(lhs, rhs) regularize tree.loc
          }
        }
        val vConn = vRhs flatMap { rhs =>
          vLhs map { lhs =>
            EntAssign(lhs, rhs) regularize tree.loc
          }
        }
        val bConn = bRhs flatMap { lhs =>
          bLhs map { rhs =>
            EntAssign(lhs, rhs) regularize tree.loc
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

  // Triple of 'payload', 'valid', 'ready' if any
  type LoweredSymbols = (Option[Symbol], Option[Symbol], Option[Symbol])

  def apply(): Pass[Pairs, Pairs] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()
    val portMaps = TrieMap[Symbol, mutable.Map[Symbol, LoweredSymbols]]()

    new EntityTransformerPass(declFirst = true, parallel = true) {
      val name = "lower-flow-control-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = {
        val portMap = mutable.Map[Symbol, LoweredSymbols]()
        portMaps(symbol) = portMap
        new LowerFlowControlA(globalReplacements, portMap)
      }
    } andThen new EntityTransformerPass(declFirst = true, parallel = true) {
      val name = "lower-flow-control-b"

      // Remap the keys to their replacements
      lazy val pMaps: collection.Map[Symbol, collection.Map[Symbol, LoweredSymbols]] =
        portMaps map { case (k, v) => (globalReplacements(k), v) }

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerFlowControlB(globalReplacements, pMaps)
    }
  }

}
