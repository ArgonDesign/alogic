////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlType
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.IteratorOps._

object ConnectChecks {

  // Return true if this is a well formed and typed Connect instance
  def apply(conn: EntConnect): Iterator[Message] = {

    // Given the type of a an expression in a connect, return the type of
    // flow control it has (if any), assuming it appears on the given side.
    def flowControlType(kind: Type, onLhs: Boolean): Option[FlowControlType] = kind match {
      case TypeIn(_, FlowControlTypeNone)         => None
      case TypeIn(_, fc)                          => Some(fc)
      case TypeOut(_, FlowControlTypeNone, _)     => None
      case TypeOut(_, fc, _)                      => Some(fc)
      case _: TypeSnoop                           => Some(FlowControlTypeReady)
      case TypePipeIn(_, FlowControlTypeNone)     => None
      case TypePipeIn(_, fc)                      => Some(fc)
      case TypePipeOut(_, FlowControlTypeNone, _) => None
      case TypePipeOut(_, fc, _)                  => Some(fc)
      case kind: TypeEntity                       =>
        // Check the cardinal port
        kind(if (onLhs) "out" else "in") flatMap { symbol => flowControlType(symbol.kind, onLhs) }
      case _ => None
    }

    def checkComplexExprWithFlowControl(expr: Expr, onLhs: Boolean): Iterator[Message] = {
      // Given an expression in a connect, extract sub-expressions that
      // reference ports.
      def portReferences(expr: Expr): Iterator[Expr] = {
        def isIO(kind: Type): Boolean = kind match {
          case _: TypeIn | _: TypeOut | _: TypePipeIn | _: TypePipeOut => true
          case _                                                       => false
        }

        expr flatCollect {
          // Simply pick up any port types
          case expr: Expr if isIO(expr.tpe) => Iterator.single(expr)
          // Select from instance that does not yield a port. Stop recursion so we
          // do not pick up the entity reference under the select.
          case ExprDot(tgt, _, _) if tgt.tpe.isEntity => unreachable // Iterator.empty
          // Reference to an entity is a reference to one of the cardinal ports
          case expr: Expr if expr.tpe.isEntity => Iterator.single(expr)
        }
      }

      expr match {
        case _: ExprSym                             => Iterator.empty // OK: simple expression
        case ExprDot(tgt, _, _) if tgt.tpe.isEntity => Iterator.empty // OK: simple expression
        case _ =>
          portReferences(expr) filter { expr =>
            flowControlType(expr.tpe, onLhs).isDefined
          } map { expr =>
            Error(expr, "Port with flow control cannot be connected in a complex expression")
          }
      }
    }

    def checkValidConnection(src: Expr, dst: Expr): Iterator[Message] = {
      def checkValidSource(expr: Expr): Iterator[Message] = {
        def error(extra: String*): Iterator[Error] = Iterator.single(
          Error(expr.loc, "Expression on left hand side of '->' is not a valid source." +: extra)
        )

        expr match {
          case ExprSym(symbol) =>
            symbol.kind match {
              case _: TypeIn | _: TypePipeIn | _: TypeConst | _: TypeParam => Iterator.empty // OK
              case _: TypeOut | _: TypePipeOut =>
                Iterator.unless(dst.tpe.isSnoop) thenIterator {
                  error(s"'${symbol.name}' is an output of an enclosing entity.")
                }
              case _: TypeSnoop =>
                Iterator.unless(dst.tpe.isSnoop) thenIterator {
                  error(s"'${symbol.name}' is a snoop port of an enclosing entity.")
                }
              case kind: TypeEntity =>
                // Check the cardinal
                Iterator.when(kind("out").isEmpty) thenIterator {
                  error(s"'${symbol.name}' has no cardinal output port.") map {
                    _ withNotes {
                      Iterator.when(!kind.symbol.desc.isInstanceOf[DescSingleton]) thenSingle {
                        Note(kind.symbol, s"'${symbol.name}' is an instance of:")
                      }
                    }
                  }
                }
              case _ => error()
            }
          case ExprDot(tgt, _, _) =>
            if (tgt.tpe.isEntity) {
              expr.tpe match {
                case _: TypeOut | _: TypePipeOut => Iterator.empty // OK
                case _: TypeIn | _: TypePipeIn =>
                  Iterator.unless(dst.tpe.isSnoop) thenIterator {
                    error("It is an input of the referenced instance.")
                  }
                case _: TypeSnoop =>
                  Iterator.unless(dst.tpe.isSnoop) thenIterator {
                    error("It is a snoop port of the referenced instance.")
                  }
                case _ => unreachable
              }
            } else if (expr.tpe.isPacked) {
              checkValidSource(tgt)
            } else {
              error()
            }
          case ExprIndex(tgt, _)       => checkValidSource(tgt)
          case ExprSlice(tgt, _, _, _) => checkValidSource(tgt)
          case ExprCat(parts)          => parts.iterator flatMap checkValidSource
          case ExprRep(_, expr)        => checkValidSource(expr)
          case _: ExprNum              => error() // TODO: allow this - Iterator.empty // Ok
          case _                       => if (expr.tpe.isPacked) Iterator.empty else error()
        }
      }

      def checkValidSink(expr: Expr): Iterator[Message] = {
        def error(extra: String*): Iterator[Error] = Iterator single {
          Error(expr.loc, "Expression on right hand side of '->' is not a valid sink." +: extra)
        }

        expr match {
          case ExprSym(symbol) =>
            val n = symbol.name
            symbol.kind match {
              case _: TypeOut | _: TypePipeOut => Iterator.empty // OK
              case kind: TypeEntity =>
                Iterator.when(kind("in").isEmpty) thenIterator {
                  error(s"'$n' has no cardinal input port.") map {
                    _ withNotes {
                      Iterator.when(!kind.symbol.desc.isInstanceOf[DescSingleton]) thenSingle {
                        Note(kind.symbol, s"'$n' is an instance of:")
                      }
                    }
                  }
                }
              case _: TypeIn | _: TypePipeIn => error(s"'$n' is an input of an enclosing entity.")
              case _                         => error()
            }
          case ExprDot(tgt, _, _) =>
            if (tgt.tpe.isEntity) {
              expr.tpe match {
                case _: TypeIn | _: TypePipeIn | _: TypeSnoop => Iterator.empty // OK
                case _: TypeOut | _: TypePipeOut =>
                  error("It is an output of the referenced instance.")
                case _ => unreachable
              }
            } else if (expr.tpe.isPacked) {
              checkValidSink(tgt)
            } else {
              error()
            }
          case ExprIndex(tgt, _)       => checkValidSink(tgt)
          case ExprSlice(tgt, _, _, _) => checkValidSink(tgt)
          case ExprCat(parts)          => parts.iterator flatMap checkValidSink
          case _                       => unreachable // Not an LValue
        }
      }

      checkComplexExprWithFlowControl(src, onLhs = true) concat
        checkComplexExprWithFlowControl(dst, onLhs = false) concat
        checkValidSource(src) concat
        checkValidSink(dst)
    }

    def checkTypesCompatible(lhs: Expr)(rhs: Expr): Iterator[Message] = {
      val lKind = lhs.tpe match {
        case kind: TypeEntity => kind("out").get.kind
        case kind             => kind
      }
      val rKind = rhs.tpe match {
        case kind: TypeEntity => kind("in").get.kind
        case kind             => kind
      }
      (lKind, rKind) match {
        case (_: TypePipeOut, _: TypePipeIn) => Iterator.empty
        case (_: TypePipeIn, _: TypePipeOut) => unreachable // Should have been caught already
        case (_: TypePipeOut, _) =>
          Iterator.single(Error(rhs, "Cannot connect pipeline port to non-pipeline port"))
        case (_, _: TypePipeIn) =>
          Iterator.single(Error(lhs, "Cannot connect non-pipeline port to pipeline port"))
        case _ =>
          val lWidth = lKind.width
          val rWidth = rKind.width
          Iterator.when(lWidth != rWidth) thenSingle {
            val loc = rhs.loc.copy(start = lhs.loc.start)
            Error(loc, s"Connected ports have mismatched widths: $lWidth -> $rWidth")
          }
      }
    }

    def checkValidStorage(rhs: Expr): Iterator[Message] = {
      def outWithNonDefaultStorage(symbol: Symbol): Boolean = symbol.kind match {
        case TypeOut(_, _, st)     => st != StorageTypeDefault
        case TypePipeOut(_, _, st) => st != StorageTypeDefault
        case _                     => false
      }
      rhs flatCollect {
        case expr @ ExprSym(symbol) if outWithNonDefaultStorage(symbol) =>
          Iterator.single(
            Error(symbol, "Output port driven by '->' cannot have a storage specifier") withNote
              Note(expr, s"'${symbol.name}' is driven here")
          )
      }
    }

    def checkNoInitializer(rhs: Expr): Iterator[Message] =
      rhs flatCollect {
        case expr @ ExprSym(symbol) if symbol.kind.isOut && symbol.desc.initializer.isDefined =>
          Iterator.single(
            Error(symbol, "Output port driven by '->' cannot have an initializer") withNote
              Note(expr, s"'${symbol.name}' is driven here")
          )
      }

    def checkFlowControlCompatible(lhs: Expr)(rhs: Expr): Iterator[Message] = {
      val fctl = flowControlType(lhs.tpe, onLhs = true)
      val fctr = flowControlType(rhs.tpe, onLhs = false)
      Iterator.when(fctl != fctr) thenSingle {
        val loc = rhs.loc.copy(start = lhs.loc.start)
        def txt(fct: Option[FlowControlType]): String = fct match {
          case None                       => "none"
          case Some(FlowControlTypeNone)  => unreachable
          case Some(FlowControlTypeValid) => "sync"
          case Some(FlowControlTypeReady) => "sync ready"
        }
        Error(loc, "Ports have incompatible flow control", s"'${txt(fctl)}' -> '${txt(fctr)}'")
      }
    }

    val EntConnect(lhs, rhss) = conn

    // Perform the checks in stages. Later stages assume earlier checks passed.
    rhss.iterator.flatMap(rhs => checkValidConnection(lhs, rhs)) ifEmpty {
      (rhss.iterator flatMap checkTypesCompatible(lhs)) ++
        (rhss.iterator flatMap checkValidStorage) ++
        (rhss.iterator flatMap checkNoInitializer)
    } ifEmpty {
      (rhss.iterator flatMap checkFlowControlCompatible(lhs))
    }
  }

}
