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
// Split structures to constituent signals
//   - Does not update instance port selects
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

final class SplitStructsA(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  private[this] val fieldIndexStack = Stack[Int]()

  private[this] def flattenStruct(prefix: String, kind: TypeStruct): List[(String, Type)] = {
    kind.fields flatMap {
      case (fName, fKind) =>
        fKind match {
          case k: TypeStruct => flattenStruct(s"${prefix}__${fName}", k)
          case other         => List((s"${prefix}__${fName}", other))
        }
    }
  }

  override def enter(tree: Tree) = tree match {
    case Decl(Sym(symbol), oKind, _) => {
      oKind.underlying match {
        case struct: TypeStruct => {
          val newSymbols = for ((fName, fKind) <- flattenStruct(symbol.name, struct)) yield {
            val nKind = oKind match {
              case k: TypeIn     => k.copy(kind = fKind)
              case k: TypeOut    => k.copy(kind = fKind)
              case k: TypeConst  => k.copy(kind = fKind)
              case _: TypeStruct => fKind
              case _             => unreachable
            }
            cc.newTermSymbol(fName, tree.loc, nKind)
          }
          symbol.attr.fieldSymbols set newSymbols
        }
        case _ => ()
      }
    }

    case ExprSelect(expr, sel) => {
      expr.tpe.underlying match {
        case kind: TypeStruct => fieldIndexStack.push(kind.fieldNames.takeWhile(_ != sel).length)
        case _                => fieldIndexStack.push(-1)
      }
    }

    case _ => ()
  }

  private[this] def fieldDecls(fSymbols: List[TermSymbol], initOpt: Option[Expr]): List[Decl] = {
    initOpt match {
      case Some(init) => {
        val widths = fSymbols map { _.denot.kind.width.value.get.toInt }
        val lsbs = widths.scanRight(0)(_ + _).tail
        val t = for ((symbol, lsb, width) <- (fSymbols, lsbs, widths).zipped) yield {
          val msb = lsb + width - 1
          val expr = ExprSlice(init, Expr(msb), ":", Expr(lsb)) regularize init.loc
          // TODO: teach simplify to simplify more things as necesary
          Decl(Sym(symbol), symbol.denot.kind, Some(expr.simplify))
        }
        t.toList
      }
      case None => {
        for (symbol <- fSymbols) yield {
          Decl(Sym(symbol), symbol.denot.kind, None)
        }
      }
    }
  }

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // ExprRef
      //////////////////////////////////////////////////////////////////////////

      case ExprRef(Sym(symbol)) => {
        // Rewrite reference to struct symbol as a nested
        // concatenation of references to the field symbols
        symbol.attr.fieldSymbols.get map { fSymbols =>
          val it = fSymbols.toIterator
          def cat(struct: TypeStruct): ExprCat = ExprCat {
            for (fType <- struct.fieldTypes) yield {
              fType match {
                case struct: TypeStruct => cat(struct)
                case _                  => ExprRef(Sym(it.next()))
              }
            }
          }
          cat(symbol.denot.kind.underlying.asInstanceOf[TypeStruct])
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // ExprRef
      //////////////////////////////////////////////////////////////////////////

      case ExprSelect(expr, _) => {
        if (fieldIndexStack.top >= 0) {
          val ExprCat(parts) = expr
          parts(fieldIndexStack.top)
        } else {
          tree
        }
      } followedBy {
        fieldIndexStack.pop()
      }

      //////////////////////////////////////////////////////////////////////////
      // Decl
      //////////////////////////////////////////////////////////////////////////

      case decl @ Decl(Sym(symbol: TermSymbol), kind, init) => {
        // Add field declarations
        symbol.attr.fieldSymbols.get map { fSymbols =>
          val fDecls = fieldDecls(fSymbols, init)
          // Keep original declarations of ports. These are used to resolve
          // inter-entity connections in a second pass
          kind match {
            case _: TypeIn  => Thicket(decl :: fDecls) regularize tree.loc
            case _: TypeOut => Thicket(decl :: fDecls) regularize tree.loc
            case _          => Thicket(fDecls) regularize tree.loc
          }
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Entity
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        // Update type of entity with new ports.
        val portSymbols = entity.declarations collect {
          case Decl(Sym(symbol: TermSymbol), _: TypeIn, _)  => symbol
          case Decl(Sym(symbol: TermSymbol), _: TypeOut, _) => symbol
        }

        val newKind = entitySymbol.denot.kind match {
          case kind: TypeEntity => kind.copy(portSymbols = portSymbols)
          case _                => unreachable
        }
        entitySymbol withDenot entitySymbol.denot.copy(kind = newKind)

        tree
      }

      case _ => tree
    }

    // If we did modify the node, regularize it
    if (result ne tree) {
      result regularize tree.loc
    }

    // Done
    result
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(fieldIndexStack.isEmpty)
  }

}
