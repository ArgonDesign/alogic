////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Lower TypeVector to TypeUInt by flattening dimensions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class LowerVectorsBase extends StatefulTreeTransformer {

  final protected val tgtTpe = mutable.Stack[Type]()

  final private var lvalueLevel = 0

  final private var catLevel = 0

  final private def fixSign(expr: Expr, makeSigned: Boolean): Expr = {
    // Turn it into a signed value if required,
    // unless it is the target of the assignment
    if (makeSigned && (lvalueLevel - catLevel) != 2) expr.castSigned else expr
  }

  final protected def convertSlice(expr: ExprSlice, tgtType: TypeVector): Expr = {
    val ExprSlice(tgt, lIdx, op, rIdx) = expr
    val TypeVector(eKind, _) = tgtType
    val ew = eKind.width
    val shape = tgt.tpe.shapeIter.next()

    val newLIdx = {
      val w = clog2(shape) max 1

      op match {
        case ":" =>
          rIdx match {
            case ExprInt(_, _, v) => ExprInt(false, w, ew * v)
            case _                => ExprInt(false, w, ew) * (rIdx zx w)
          }
        case "+:" =>
          lIdx match {
            case ExprInt(_, _, v) => ExprInt(false, w, ew * v)
            case _                => ExprInt(false, w, ew) * (lIdx zx w)
          }
        case "-:" =>
          lIdx match {
            case ExprInt(_, _, v) =>
              ExprInt(false, w, ew * (v - rIdx.value.toInt + 1))
            case _ => ExprInt(false, w, ew) * ((lIdx zx w) - rIdx.value.toInt + 1)
          }
      }
    }

    val newRIdx = {
      val sliceWidth = op match {
        case ":" => lIdx.value - rIdx.value + 1
        case _   => rIdx.value
      }

      ExprInt(false, clog2(shape + 1), ew * sliceWidth) regularize lIdx.loc
    }

    ExprSlice(tgt, newLIdx, "+:", newRIdx) regularize tgt.loc
  }

  final protected def convertIndex(expr: ExprIndex, tgtType: TypeVector): Expr = {
    val ExprIndex(tgt, index) = expr
    val TypeVector(eKind, size) = tgtType
    if (size == 1) {
      fixSign(tgt, eKind.isSigned)
    } else {
      val ew = eKind.width
      val shape = tgt.tpe.shapeIter.next()

      val newLIdx = {
        val w = clog2(shape) max 1
        index match {
          case ExprInt(_, _, v) => ExprInt(false, w, ew * v)
          case _                => ExprInt(false, w, ew) * (index zx w)
        }
      }

      val newRIdx = ExprInt(false, clog2(shape + 1), ew)

      fixSign(ExprSlice(tgt, newLIdx, "+:", newRIdx) regularize tgt.loc, eKind.isSigned)
    }
  }

  final protected def preEnter(tree: Tree): Unit = tree match {
    case _: StmtAssign => lvalueLevel += 1

    case expr: Expr =>
      if (lvalueLevel > 0) {
        lvalueLevel += 1
        if (expr.isInstanceOf[ExprCat]) {
          catLevel += 1
        }
      }
      expr match {
        case ExprIndex(tgt, _)       => tgtTpe.push(tgt.tpe.underlying)
        case ExprSlice(tgt, _, _, _) => tgtTpe.push(tgt.tpe.underlying)
        case _: Expr                 => tgtTpe.push(TypeMisc)
      }

    case _ =>
  }

  final protected def postTransform(tree: Tree): Unit = tree match {
    case expr: Expr =>
      tgtTpe.pop()
      if (lvalueLevel > 0 && expr.isInstanceOf[ExprCat]) {
        catLevel -= 1
      }
      if (lvalueLevel > 2) {
        lvalueLevel -= 1
      } else {
        lvalueLevel = 0
      }
    case _ =>
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(tgtTpe.isEmpty)
    assert(lvalueLevel == 0)
    assert(catLevel == 0)
  }

}

final class LowerVectorsA(globalReplacements: TrieMap[Symbol, Symbol]) extends LowerVectorsBase {

  override def replace(symbol: Symbol): Boolean =
    enclosingSymbols.isEmpty || symbol.kind.underlying.isVector

  override def enter(tree: Tree): Option[Tree] = {
    preEnter(tree)
    tree match {
      case DeclEntity(symbol, _) => globalReplacements(orig(symbol)) = symbol
      case _                     =>
    }
    None
  }

  private def vecSpec(symbol: Symbol): Expr = {
    assert(symbol.kind.underlying.isVector)
    ExprType(TypeUInt(symbol.kind.width))
  }

  private def vectShape(kind: TypeVector): (List[Long], Boolean) = kind.kind match {
    case eKind: TypeVector =>
      val (dims, signed) = vectShape(eKind)
      (kind.size :: dims, signed)
    case eKind =>
      (kind.size :: Nil, eKind.isSigned)
  }

  override def transform(tree: Tree): Tree = tree pipe {
    // TODO: handle arrays as well

    //////////////////////////////////////////////////////////////////////////
    // Update declarations of vectors
    //////////////////////////////////////////////////////////////////////////

    case decl @ DeclVar(symbol, _) if symbol.kind.underlying.isVector =>
      decl.copy(spec = vecSpec(symbol)) regularize tree.loc

    case decl @ DeclIn(symbol, _, _) if symbol.kind.underlying.isVector =>
      symbol.attr.vectShape set vectShape(symbol.kind.underlying.asVector)
      decl.copy(spec = vecSpec(symbol)) regularize tree.loc

    case decl @ DeclOut(symbol, _, _, _) if symbol.kind.underlying.isVector =>
      symbol.attr.vectShape set vectShape(symbol.kind.underlying.asVector)
      decl.copy(spec = vecSpec(symbol)) regularize tree.loc

    case decl @ DeclConst(symbol, _) if symbol.kind.underlying.isVector =>
      decl.copy(spec = vecSpec(symbol)) regularize tree.loc

    //////////////////////////////////////////////////////////////////////////
    // Transform vector operations on plain vector variables
    //////////////////////////////////////////////////////////////////////////

    // Slice
    case expr @ ExprSlice(tgt, _, _, _) =>
      tgtTpe.top match {
        case kind: TypeVector =>
          // By this point the target should not have a plain Vector type
          assert(!tgt.tpe.isVector)
          // Convert only if a plain vector (not an input/output/const/etc)
          if (!tgt.tpe.underlying.isVector) {
            convertSlice(expr, kind)
          } else {
            tree
          }
        case _ => tree
      }

    // Index
    case expr @ ExprIndex(tgt, _) =>
      tgtTpe.top match {
        case kind: TypeVector =>
          // By this point the target should not have a plain Vector type
          assert(!tgt.tpe.isVector)
          // Convert only if a plain vector (not an input/output/const/etc)
          if (!tgt.tpe.underlying.isVector) {
            convertIndex(expr, kind)
          } else {
            tree
          }
        case _ => tree
      }

    // Cast
    case expr @ ExprCast(kind: TypeVector, _) =>
      TypeAssigner(expr.copy(kind = TypeUInt(kind.width)) withLocOf expr)

    //
    case _ => tree
  } tap { _ =>
    postTransform(tree)
  }

}

final class LowerVectorsB(globalReplacements: scala.collection.Map[Symbol, Symbol])
    extends LowerVectorsBase {

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  override protected def enter(tree: Tree): Option[Tree] = {
    preEnter(tree)
    None
  }

  override def transform(tree: Tree): Tree = tree pipe {
    //////////////////////////////////////////////////////////////////////////
    // Update instance types
    //////////////////////////////////////////////////////////////////////////

    case decl @ DeclInstance(_, ExprSym(eSymbol), _) =>
      globalReplacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    //////////////////////////////////////////////////////////////////////////
    // Transform vector operations on symbols with a vector as underlying
    //////////////////////////////////////////////////////////////////////////

    // Slice
    case expr @ ExprSlice(tgt, _, _, _) =>
      tgtTpe.top match {
        case kind: TypeVector =>
          // By this point the target should not have an underlying Vector type
          assert(!tgt.tpe.underlying.isVector)
          convertSlice(expr, kind)
        case _ => tree
      }

    // Index
    case expr @ ExprIndex(tgt, _) =>
      tgtTpe.top match {
        case kind: TypeVector =>
          // By this point the target should not have an underlying Vector type
          assert(!tgt.tpe.underlying.isVector)
          convertIndex(expr, kind)
        case _ => tree
      }

    //
    case _ => tree
  } tap { _ =>
    postTransform(tree)
  }

  override def finish(tree: Tree): Tree = {
    tree match {
      case defn: DefnEntity =>
        val icos = defn.symbol.attr.interconnectClearOnStall.getOrElse(Nil)
        defn.symbol.attr.interconnectClearOnStall set {
          icos map {
            case (symbol, name) => (repl(symbol).getOrElse(symbol), name)
          }
        }
      case _ =>
    }
    tree
  }

  override def finalCheck(tree: Tree): Unit = {
    super.finalCheck(tree)
    // $COVERAGE-OFF$ Debug code
    tree visit {
      case t: Tree if t.tpe.underlying.isVector =>
        throw Ice(t, "Tree with vector type remains", t.toSource)
    }
    // $COVERAGE-ON$
  }

}

object LowerVectors {

  def apply(): Pass[Pairs, Pairs] = {
    val globalReplacements = TrieMap[Symbol, Symbol]()

    new EntityTransformerPass(declFirst = true) {
      val name = "lower-vectors-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerVectorsA(globalReplacements)
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "lower-vectors-b"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerVectorsB(globalReplacements)
    }
  }

}
