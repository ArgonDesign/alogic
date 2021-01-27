////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  The purpose of the Clarify transformation is to take a tree typed by the
//  TypeChecker, and transform it such that each node can be typed in isolation
//  without knowing anything about the containing tree. The TypeChecker assigns
//  some types based on context, which are different from the types that would
//  be assigned by the TypeAssigner, which does not consider context. After the
//  Clarify transform, the TypeAssigner should yield the same type for all nodes
//  within the Tree.
//
//  Additionally, the Clarify transform also ensures that the type of an
//  initializer expression in a definition is the same as the underlying type
//  of the symbol being defined, hence the initializer can be substituted for
//  the symbol for evaluation purposes.
//
//  Furthermore, the TypeAssigner cannot compute values of symbols yet, so
//  Clarify will evaluate expressions that are required by TypeAssigner during
//  type computation.
//
//  One notable exception that Clarify cannot handle is references to
//  specializations of parameterized user defined types. These need to be
//  replaced globally in order to keep references well typed, and is handled
//  by the Finalize transform.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.TypeCompound
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.Types.TypeCallable
import com.argondesign.alogic.core.Types.TypeCtrlFunc
import com.argondesign.alogic.core.Types.TypeInt
import com.argondesign.alogic.core.Types.TypeNum
import com.argondesign.alogic.core.Types.TypeType
import com.argondesign.alogic.core.Types.TypeUInt
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.util.unreachable

object Clarify {

  def apply[T <: Tree](tree: T)(implicit fe: Frontend): T = Clarify(tree, None)

  private def apply[T <: Tree](
      tree: T,
      enclosingFunctionType: Option[TypeCallable]
    )(
      implicit
      fe: Frontend
    ): T = {
    require(tree.hasTpe)
    require(!tree.tpe.isError)

    tree rewrite new StatelessTreeTransformer {
      private def cast(kind: Type, expr: Expr): ExprCast = expr cast kind.underlying.asFund

      private def castToMatchWidth(expr: Expr, kind: Type): ExprCast = {
        require(kind.isPacked)
        require(expr.tpe.underlying.isNum)
        expr cast TypeInt(expr.tpe.isSigned, kind.width)
      }

      override protected def skip(tree: Tree): Boolean = tree match {
        case _: DescParametrized => true
        case _                   => false
      }

      override protected def enter(tree: Tree): Option[Tree] = tree pipe {
        ////////////////////////////////////////////////////////////////////////
        // Descend Function definitions with appropriate context
        ////////////////////////////////////////////////////////////////////////

        case desc @ DescFunc(_, _, _, ret, args, body) =>
          def clarify[R <: Tree](tree: R): R = Clarify(tree, Some(desc.symbol.kind.asCallable))
          Some {
            desc.copy(
              ret = clarify(ret),
              args = args map clarify,
              body = body map clarify
            )
          }

        ////////////////////////////////////////////////////////////////////////
        // Convert ExprDot to ExprSymSel, now that it has been type checked
        ////////////////////////////////////////////////////////////////////////

        case ExprDot(tgt, sel, idxs) =>
          assert(idxs.isEmpty) // Indices incorporated into selector by ResolveNames
          val newTgt = walkSame(tgt)
          val symbol = newTgt.tpe match {
            case k: TypeScope              => k.scoped(sel).get
            case TypeType(k: TypeCompound) => k(sel).get
            case TypeNone(k: TypeCompound) => k(sel).get
            case k: TypeCompound           => k(sel).get
            case _                         => unreachable
          }
          Some(ExprSymSel(newTgt, symbol))

        ////////////////////////////////////////////////////////////////////////
        // Clarify invocations of builtins
        ////////////////////////////////////////////////////////////////////////

        case expr: ExprBuiltin => Some(expr.builtin.clarify(expr))

        ////////////////////////////////////////////////////////////////////////
        // Ignore arguments in parameter specialization calls
        ////////////////////////////////////////////////////////////////////////

        case expr @ ExprCall(tgt, _) if tgt.tpe.isParametrized =>
          // Don't descend into argument list as unary ticks in there cannot be
          // resolved out of context. These calls will be replaced at the end
          // of elaboration with a reference to the specialized symbol anyway.
          Some(expr.copy(expr = walkSame(tgt)) withLocOf tree withTpe tree.tpe)

        ////////////////////////////////////////////////////////////////////////
        // Replace calls to int/uint with the sized type
        ////////////////////////////////////////////////////////////////////////

        case ExprCall(ExprType(_: TypeNum), _) =>
          // The TypeChecker already proved this is well formed and assigned
          // the type of this tree, so just grab the result type from there
          tree.tpe match {
            case TypeType(kind) => Some(ExprType(kind))
            case _              => unreachable
          }

        ////////////////////////////////////////////////////////////////////////
        // Replace unary tick operators with cast
        ////////////////////////////////////////////////////////////////////////

        case ExprUnary("'", op) => Some(ExprCast(tree.tpe.asFund, walkSame(op)))

        ////////////////////////////////////////////////////////////////////////
        // Evaluate expressions that must be constant for TypeAssigner. Note
        // these have already been checked by the TypeChecker, so they are
        // always computable at this point
        ////////////////////////////////////////////////////////////////////////

        case ExprRep(count, tgt) =>
          Some {
            val newCount =
              ExprNum(false, fe.evaluate(count, unreachable).get) regularize count.loc
            val newTgt = walkSame(tgt)
            transform(TypeAssigner(ExprRep(newCount, newTgt) withLocOf tree))
          }

        case ExprIndex(tgt, idx) if tgt.tpe.isType =>
          Some {
            val newTgt = walkSame(tgt)
            val newIdx = ExprNum(false, fe.evaluate(idx, unreachable).get) regularize idx.loc
            transform(TypeAssigner(ExprIndex(newTgt, newIdx) withLocOf tree))
          }

        case ExprSlice(tgt, lIdx, ":", rIdx) =>
          Some {
            val newTgt = walkSame(tgt)
            val newLIdx =
              ExprNum(false, fe.evaluate(lIdx, unreachable).get) regularize lIdx.loc
            val newRIdx =
              ExprNum(false, fe.evaluate(rIdx, unreachable).get) regularize rIdx.loc
            transform(TypeAssigner(ExprSlice(newTgt, newLIdx, ":", newRIdx) withLocOf tree))
          }

        case ExprSlice(tgt, lIdx, op, rIdx) =>
          Some {
            val newTgt = walkSame(tgt)
            val newLIdx = walkSame(lIdx)
            val newRIdx =
              ExprNum(false, fe.evaluate(rIdx, unreachable).get) regularize rIdx.loc
            transform(TypeAssigner(ExprSlice(newTgt, newLIdx, op, newRIdx) withLocOf tree))
          }

        case ExprBinary(lhs, "'", rhs) =>
          Some {
            val newLhs = ExprNum(false, fe.evaluate(lhs, unreachable).get) regularize lhs.loc
            val newRhs = walkSame(rhs)
            transform(TypeAssigner(ExprBinary(newLhs, "'", newRhs) withLocOf tree))
          }

        //
        case _ => None
      } tap {
        case Some(result) if !result.hasTpe => TypeAssigner(result withLocOf tree)
        case _                              => //
      }

      override protected def transform(tree: Tree): Tree = tree pipe {
        ////////////////////////////////////////////////////////////////////////
        // Fix types of initializer expressions in definitions. Also evaluate
        // expressions in Desc nodes that must be constant for DescToDeclDefn.
        // Note these have already been checked by the TypeChecker, so they are
        // always computable at this point.
        ////////////////////////////////////////////////////////////////////////

        case desc @ Desc(Sym(symbol)) =>
          desc.initializer flatMap { init =>
            Option.when(symbol.kind.underlying != init.tpe.underlying) {
              // Definition of packed symbol with unsized integer initializer
              val newInit = cast(symbol.kind, init)
              desc match {
                case d: DescVar    => d.copy(initOpt = Some(newInit))
                case d: DescVal    => d.copy(init = newInit)
                case d: DescStatic => d.copy(initOpt = Some(newInit))
                case d: DescOut    => d.copy(initOpt = Some(newInit))
                case d: DescParam  => d.copy(initOpt = Some(newInit))
                case d: DescConst  => d.copy(init = newInit)
                case d: DescGenVar => d.copy(init = newInit)
                case _             => unreachable
              }
            }
          } getOrElse tree pipe {
            case d: DescArray =>
              d.copy(size =
                TypeAssigner(ExprNum(false, fe.evaluate(d.size, unreachable).get) withLocOf d.size)
              )
            case d: DescSram =>
              d.copy(size =
                TypeAssigner(ExprNum(false, fe.evaluate(d.size, unreachable).get) withLocOf d.size)
              )
            case other => other
          }

        ////////////////////////////////////////////////////////////////////////
        // Cast unsized integers to the context determined width
        ////////////////////////////////////////////////////////////////////////

        case stmt @ StmtAssign(lhs, rhs) if lhs.tpe.isPacked && rhs.tpe.underlying.isNum =>
          stmt.copy(rhs = castToMatchWidth(rhs, lhs.tpe))

        case stmt @ StmtUpdate(lhs, "&" | "|" | "^" | "*" | "/" | "%" | "+" | "-", rhs)
            if lhs.tpe.isPacked && rhs.tpe.underlying.isNum =>
          stmt.copy(rhs = castToMatchWidth(rhs, lhs.tpe))

        case stmt @ StmtReturn(_, Some(expr)) =>
          enclosingFunctionType flatMap { kind =>
            Option.when(kind.retType.isPacked && expr.tpe.underlying.isNum) {
              stmt.copy(exprOpt = Some(castToMatchWidth(expr, kind.retType)))
            }
          } getOrElse tree

        case expr @ ExprIndex(tgt, idx) if idx.tpe.underlying.isNum =>
          tgt.tpe.shapeIter.nextOption() map { size =>
            expr.copy(index = cast(TypeUInt(clog2(size) max 1), idx))
          } getOrElse tree

        case expr @ ExprSlice(tgt, lIdx, op, rIdx)
            if lIdx.tpe.underlying.isNum || rIdx.tpe.underlying.isNum =>
          tgt.tpe.shapeIter.nextOption() map { size =>
            val lWidth = clog2(size) max 1
            val rWidth = if (op == ":") lWidth else clog2(size + 1)
            val newLIdx = if (lIdx.tpe.underlying.isNum) cast(TypeUInt(lWidth), lIdx) else lIdx
            val newRIdx = if (rIdx.tpe.underlying.isNum) cast(TypeUInt(rWidth), rIdx) else rIdx
            expr.copy(lIdx = newLIdx, rIdx = newRIdx)
          } getOrElse tree

        case expr @ ExprCall(func, args) =>
          val kinds = func.tpe match {
            case TypeCombFunc(_, _, argTypes)     => argTypes
            case TypeCtrlFunc(_, _, argTypes)     => argTypes
            case TypeXenoFunc(_, _, argTypes)     => argTypes
            case TypeStaticMethod(_, _, argTypes) => argTypes
            case TypeNormalMethod(_, _, argTypes) => argTypes
            case _                                => unreachable
          }

          val needsCasts = kinds zip args map {
            case (k, a: ArgP) => k.isPacked && a.expr.tpe.underlying.isNum
            case _            => unreachable
          }

          if (needsCasts exists identity) {
            val newArgs = List from {
              for {
                (needsCast, kind, arg) <- needsCasts lazyZip kinds lazyZip args
              } yield {
                if (needsCast) {
                  arg match {
                    case ArgP(e) =>
                      TypeAssigner(ArgP(castToMatchWidth(e, kind)) withLoc arg.loc)
                    case _ => unreachable
                  }
                } else arg
              }
            }
            expr.copy(args = newArgs)
          } else {
            tree
          }

        case expr @ ExprBinary(
              lhs,
              "&" | "|" | "^" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<=" | "==" | "!=",
              rhs
            ) if lhs.tpe.underlying.isNum != rhs.tpe.underlying.isNum =>
          if (lhs.tpe.underlying.isNum) {
            expr.copy(lhs = castToMatchWidth(lhs, rhs.tpe))
          } else {
            expr.copy(rhs = castToMatchWidth(rhs, lhs.tpe))
          }

        case expr @ ExprCond(_, tExpr, eExpr)
            if tExpr.tpe.underlying.isNum != eExpr.tpe.underlying.isNum =>
          if (tExpr.tpe.underlying.isNum) {
            expr.copy(thenExpr = castToMatchWidth(tExpr, eExpr.tpe))
          } else {
            expr.copy(elseExpr = castToMatchWidth(eExpr, tExpr.tpe))
          }

        //
        case _ => tree
      } tap {
        case result if result ne tree => TypeAssigner(result withLocOf tree)
        case _                        => // Not transformed
      }

      override def finalCheck(tree: Tree): Unit = {
        // $COVERAGE-OFF$ Debug code
        tree visit {
          case _: DescParametrized                        => // Ok
          case ExprCall(tgt, _) if tgt.tpe.isParametrized => finalCheck(tgt)
          case node @ ExprUnary("'", _) =>
            throw Ice(node, s"Clarify: Unary ' remains")
          case DescConst(Sym(s), _, _, init) if init.tpe.underlying != s.kind.underlying =>
            throw Ice(init, s"Clarify: bad initializer type ${s.kind.underlying}  vs ${init.tpe}")
        }
        // $COVERAGE-ON$
      }
    }
  }

}
