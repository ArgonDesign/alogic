////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Lower foreign functions. Note: This really is target language specific, but
// we only output Verilog at this point, so the implementation assumes that
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Json
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.SequenceNumbers

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerForeignFunctions(
    encountered: mutable.Map[String, (TypeXenoFunc, List[String])]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  // TODO: change signatures to remove Record/Vector
  private val extraStmts = mutable.Stack[ListBuffer[Stmt]]()
  private val extraSymbols = new ListBuffer[Symbol]

  private val sequenceNumbers = mutable.Map[String, SequenceNumbers]()

  override def enter(tree: Tree): Option[Tree] = tree match {
    case DeclFunc(symbol, FuncVariant.Xeno, _, args) =>
      encountered synchronized {
        encountered.get(symbol.name) match {
          case None =>
            encountered(symbol.name) = (symbol.kind.asXenoFunc, args map { _.symbol.name })
          case Some((kind, _)) =>
            val newKind = symbol.kind.asXenoFunc
            if (kind.retType != newKind.retType || kind.argTypes != newKind.argTypes) {
              cc.error(
                tree,
                "Foreign function imported with different signatures. First import is at:",
                kind.symbol.loc.prefix
              )
            }
        }
      }
      None

    case ExprCall(func, _) if func.tpe.isXenoFunc && extraStmts.isEmpty =>
      cc.error(tree, "Foreign function call can only appear inside statements.")
      Some(tree)

    case _: Stmt =>
      extraStmts.push(new ListBuffer[Stmt])
      None

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Translate calls to foreign functions
    ////////////////////////////////////////////////////////////////////////////

    case ExprCall(func, args) if func.tpe.isXenoFunc =>
      val TypeXenoFunc(fSymbol, retType, _) = func.tpe.asXenoFunc
      if (retType.isVoid) {
        // Void return type is fine as it is
        tree
      } else {
        // Otherwise create a symbol which is assigned the output value
        val tag = s"_${fSymbol.name}${cc.sep}result"
        val n = sequenceNumbers.getOrElseUpdate(tag, new SequenceNumbers).next
        val retSymbol = cc.newSymbol(s"${tag}_$n", tree.loc)
        retSymbol.kind = retType
        retSymbol.attr.combSignal set true
        // Add extra symbol
        extraSymbols append retSymbol
        // Add the call statement
        val result = TypeAssigner(ExprSym(retSymbol) withLoc tree.loc)
        extraStmts.head append {
          TypeAssigner(StmtOutcall(result, func, args map { _.expr }) withLoc tree.loc)
        }
        // Replace with the call result
        result
      }

    ////////////////////////////////////////////////////////////////////////////
    // Add extra statements
    ////////////////////////////////////////////////////////////////////////////

    case stmt: Stmt =>
      val extra = extraStmts.pop()
      if (extra.isEmpty) tree else Thicket(extra.appended(stmt).toList)

    ////////////////////////////////////////////////////////////////////////////
    // Add extra symbol decl/defn
    ////////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity if extraSymbols.nonEmpty =>
      val newBody = List from {
        defn.body.iterator concat {
          extraSymbols.iterator map { symbol =>
            EntDefn(symbol.mkDefn) regularize symbol.loc
          }
        }
      }
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case decl: DeclEntity if extraSymbols.nonEmpty =>
      val newDecls = List from {
        decl.decls.iterator concat {
          extraSymbols.iterator map { symbol =>
            symbol.mkDecl regularize symbol.loc
          }
        }
      }
      TypeAssigner(decl.copy(decls = newDecls) withLoc tree.loc)

    //
    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)
  }

}

object LowerForeignFunctions {

  def apply(): Pass[List[(Decl, Defn)], List[(Decl, Defn)]] = {

    val encountered = mutable.Map[String, (TypeXenoFunc, List[String])]()

    new EntityTransformerPass(declFirst = false) {
      val name = "lower-foreign-functions"

      override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerForeignFunctions(encountered)

      override def finish(
          pairs: List[(Decl, Defn)]
        )(
          implicit
          cc: CompilerContext
        ): List[(Decl, Defn)] = {

        def desc(kind: Type) = Iterator(
          "width" -> kind.width.toInt,
          "signed" -> kind.isSigned
        )

        val foreignFunctions = ListMap from {
          encountered.toSeq.sortBy(_._1) map {
            case (k, (kind, ids)) =>
              k -> ListMap(
                "return" -> ListMap.from(desc(kind.retType)),
                "args" -> List.from(
                  (kind.argTypes lazyZip ids) map {
                    case (kind, id) => ListMap.from(Iterator.single("name" -> id) concat desc(kind))
                  }
                )
              )
          }
        }

        if (foreignFunctions.nonEmpty) {
          val w = cc.settings.outputWriterFactory(Right("foreign_functions.json"))
          Json.write(w, foreignFunctions)
          w.close()
        }

        pairs
      }
    }
  }

}
