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
// Split structures to constituent signals
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

final class SplitStructsA(
    globalReplacements: mutable.Map[Symbol, Symbol],
    fieldMap: mutable.Map[Symbol, List[Symbol]]
)(implicit cc: CompilerContext)
    extends TreeTransformer {

  private[this] def flattenStruct(prefix: String, kind: TypeRecord): List[(String, TypeFund)] = {
    kind.publicSymbols flatMap { symbol =>
      symbol.kind match {
        case k: TypeRecord => flattenStruct(s"$prefix${cc.sep}${symbol.name}", k)
        case k: TypeFund   => List((s"$prefix${cc.sep}${symbol.name}", k))
        case k             => cc.ice(symbol, s"Don't know how to flatten field of type $k")
      }
    }
  }

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
      case decl @ DeclEntity(symbol, _) =>
        val eSymbol = orig(symbol)
        globalReplacements(eSymbol) = symbol

        for (Decl(symbol) <- decl.decls) {
          symbol.kind.underlying match {
            case struct: TypeRecord =>
              val newSymbols = for ((fName, fKind) <- flattenStruct(symbol.name, struct)) yield {
                val nKind = symbol.kind match {
                  case k: TypeIn     => k.copy(kind = fKind)
                  case k: TypeOut    => k.copy(kind = fKind)
                  case k: TypeConst  => k.copy(kind = fKind)
                  case _: TypeRecord => fKind
                  case _             => unreachable
                }
                cc.newSymbol(fName, symbol.loc) tap { s =>
                  s.kind = nKind
                  symbol.attr.payloadOfPort.get foreach {
                    s.attr.payloadOfPort.set
                  }
                }
              }
              val widths = newSymbols map { _.kind.width }
              val offsets = widths.scanLeft(BigInt(0))(_ + _)
              for ((newSymbol, offset) <- newSymbols lazyZip offsets) {
                newSymbol.attr.fieldOffset set offset.toInt
              }
              fieldMap(symbol) = newSymbols
            //
            case _ =>
          }
        }

      //
      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Replace Decl/Defn with field Decls/Defns
    //////////////////////////////////////////////////////////////////////////

    case Decl(symbol) =>
      fieldMap.get(symbol) map { fSymbols =>
        Thicket(fSymbols map { _.mkDecl }) regularize tree.loc
      } getOrElse {
        tree
      }

    case defn @ Defn(symbol) =>
      fieldMap.get(symbol) map { fSymbols =>
        val defns = defn.initializer match {
          case Some(init) =>
            val widths = fSymbols map { _.kind.width }
            val lsbs = widths.scanRight(BigInt(0))(_ + _).tail
            List from {
              for ((symbol, lsb, width) <- fSymbols lazyZip lsbs lazyZip widths) yield {
                val msb = lsb + width - 1
                val expr = init.slice(msb.toInt, ":", lsb.toInt) regularize init.loc
                symbol.mkDefn(expr.simplify)
              }
            }
          case None => fSymbols map { _.mkDefn }
        }
        Thicket(defns) regularize tree.loc
      } getOrElse {
        tree
      }

    //
    case _ => tree
  }

}

final class SplitStructsB(
    globalReplacements: collection.Map[Symbol, Symbol],
    fieldMaps: collection.Map[Symbol, collection.Map[Symbol, List[Symbol]]]
)(implicit cc: CompilerContext)
    extends TreeTransformer {

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  // Transform the target expression of ExprSelect, out of order in order to
  // avoid creating an illegal ExprSelect during the standard traversal
  override def enter(tree: Tree): Option[Tree] = tree match {
    // Select on struct
    case ExprSelect(expr, sel, _) if expr.tpe.underlying.isRecord =>
      Some {
        val kind = expr.tpe.underlying.asRecord
        val fieldIndex = kind.publicSymbols.indexWhere(_.name == sel)
        walk(expr).asInstanceOf[ExprCat].parts(fieldIndex)
      }

    // Select on instance
    case ExprSelect(expr @ ExprSym(iSymbol), sel, _) =>
      val kind = iSymbol.kind.asEntity
      val pSymbol = kind(sel).get
      val fieldMap = fieldMaps(globalReplacements(kind.symbol))
      fieldMap.get(pSymbol) map { fSymbols =>
        val ref = walk(expr).asInstanceOf[Expr]
        def cat(struct: TypeRecord, it: Iterator[Symbol]): ExprCat = ExprCat {
          struct.publicSymbols map { symbol =>
            symbol.kind match {
              case struct: TypeRecord => cat(struct, it)
              case _                  => ExprSelect(ref, it.next().name, Nil)
            }
          }
        }
        cat(pSymbol.kind.underlying.asRecord, fSymbols.iterator) regularize tree.loc
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
    // Rewrite reference to struct symbol as a nested concatenation
    //////////////////////////////////////////////////////////////////////////

    case ExprSym(symbol) =>
      fieldMaps(entitySymbol).get(symbol) map { fSymbols =>
        def cat(struct: TypeRecord, it: Iterator[Symbol]): ExprCat = ExprCat {
          struct.publicSymbols map { symbol =>
            symbol.kind match {
              case struct: TypeRecord => cat(struct, it)
              case _                  => ExprSym(it.next())
            }
          }
        }
        cat(symbol.kind.underlying.asRecord, fSymbols.iterator) regularize tree.loc
      } getOrElse tree

    //
    case _ => tree
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
}

object SplitStructs {

  def apply(): Pass[List[(Decl, Defn)], List[(Decl, Defn)]] = {

    val globalReplacements = TrieMap[Symbol, Symbol]()
    val fieldMaps = TrieMap[Symbol, mutable.Map[Symbol, List[Symbol]]]()

    new EntityTransformerPass(declFirst = true) {
      val name = "split-structs-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = {
        val fieldMap = mutable.Map[Symbol, List[Symbol]]()
        fieldMaps(symbol) = fieldMap
        new SplitStructsA(globalReplacements, fieldMap)
      }
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "split-structs-b"

      // Remap the keys to their replacements
      lazy val fMaps: collection.Map[Symbol, collection.Map[Symbol, List[Symbol]]] =
        fieldMaps map { case (k, v) => (globalReplacements(k), v) }

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new SplitStructsB(globalReplacements, fMaps)
    }
  }
}
