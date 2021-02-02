////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Lower foreign functions. Note: This really is target language specific, but
// we only output Verilog at this point, so the implementation assumes that
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.SequenceNumbers

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordered.orderingToOrdered

final class LowerForeignFunctions(
    encountered: mutable.Map[String, (TypeXenoFunc, List[String])]
  )(
    implicit
    cc: CompilerContext)
    extends StatelessTreeTransformer {

  // TODO: change signatures to remove Record/Vector
  private val extraStmts = mutable.Stack[ListBuffer[Stmt]]()
  private val extraSymbols = new ListBuffer[Symbol]

  private val sequenceNumbers = mutable.Map[String, SequenceNumbers]()

  override def enter(tree: Tree): Option[Tree] = tree match {
    case DeclFunc(symbol, FuncVariant.Xeno, _, args) =>
      // TODO: TrieMap + putIfAbsent
      encountered synchronized {
        encountered.get(symbol.name) match {
          case None =>
            encountered(symbol.name) = (symbol.kind.asXenoFunc, args map { _.symbol.name })
          case Some((kind, _)) =>
            val newKind = symbol.kind.asXenoFunc
            if (kind.retType != newKind.retType || kind.argTypes != newKind.argTypes) {
              val (a, b) =
                if (kind.symbol < symbol) (kind.symbol, symbol) else (symbol, kind.symbol)
              cc.addMessage(
                Error(b, "Foreign function imported with different signatures.") withNote
                  Note(a, "Conflicting 'import' is here")
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
            EntSplice(symbol.mkDefn) regularize symbol.loc
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

  def apply(): PairsTransformerPass = {

    val encountered = mutable.Map[String, (TypeXenoFunc, List[String])]()

    new PairTransformerPass(parallel = true) {
      val name = "lower-foreign-functions"

      override protected def transform(
          decl: Decl,
          defn: Defn
        )(
          implicit
          cc: CompilerContext
        ): (Tree, Tree) = {
        val transform = new LowerForeignFunctions(encountered)
        val newDefn = transform(defn)
        val newDecl = transform(decl)
        (newDecl, newDefn)
      }

      override def finish(pairs: Pairs)(implicit cc: CompilerContext): Pairs = {

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
          cc.manifest("foreign-functions") = foreignFunctions
        }

        pairs
      }
    }
  }

}
