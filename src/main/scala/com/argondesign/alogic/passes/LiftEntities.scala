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
// Lift nested entities, wire through directly accessed ports
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Names.TypeName
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.ValueMap
import com.argondesign.alogic.util.unreachable

final class LiftEntities(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy
    with ValueMap {

  // TODO: only works for single nesting
  // TODO: Pass down params
  // TODO: Pass down consts

  // ports declared in outer entities
  val outerIPortSymbols: Stack[Set[TermSymbol]] = Stack(Set())
  val outerOPortSymbols: Stack[Set[TermSymbol]] = Stack(Set())

  // new ports that need to be created to connect up to directly accessed outer port
  val freshIPortSymbols: Stack[Map[TermSymbol, TermSymbol]] = Stack()
  val freshOPortSymbols: Stack[Map[TermSymbol, TermSymbol]] = Stack()

  // new ports that need to be connected in this entity
  val freshIConnSymbols: Stack[Set[(TermSymbol, TypeSymbol)]] = Stack(Set())
  val freshOConnSymbols: Stack[Set[(TypeSymbol, TermSymbol)]] = Stack(Set())

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      //////////////////////////////////////////////////////////////////////////
      // Collect outer ports we are referencing
      //////////////////////////////////////////////////////////////////////////

      val referencedSymbols = {
        val it = entity collectAll {
          case ExprRef(Sym(symbol: TermSymbol)) => symbol
        }
        it.toSet
      }

      val newIPortSymbols = {
        val pairs = for {
          outerSymbol <- referencedSymbols if outerIPortSymbols.top contains outerSymbol
        } yield {
          val innerSymbol = cc.newSymbolLike(outerSymbol)
          (outerSymbol -> innerSymbol)
        }
        pairs.toMap
      }
      freshIPortSymbols.push(newIPortSymbols)

      val newOPortSymbols = {
        val pairs = for {
          outerSymbol <- referencedSymbols if outerOPortSymbols.top contains outerSymbol
        } yield {
          val innerSymbol = cc.newSymbolLike(outerSymbol)
          (outerSymbol -> innerSymbol)
        }
        pairs.toMap
      }
      freshOPortSymbols.push(newOPortSymbols)

      //////////////////////////////////////////////////////////////////////////
      // Push ports declared by us
      //////////////////////////////////////////////////////////////////////////

      val newISymbols = entity.declarations collect {
        case Decl(Sym(symbol: TermSymbol), _: TypeIn, _) => symbol
      }
      outerIPortSymbols.push(newISymbols.toSet | outerIPortSymbols.top)

      val newOSymbols = entity.declarations collect {
        case Decl(Sym(symbol: TermSymbol), _: TypeOut, _) => symbol
      }
      outerOPortSymbols.push(newOSymbols.toSet | outerOPortSymbols.top)

      //////////////////////////////////////////////////////////////////////////
      // Push placeholder empty map for fresh connections
      //////////////////////////////////////////////////////////////////////////

      freshIConnSymbols.push(Set())
      freshOConnSymbols.push(Set())
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity => {
      entity valueMap { entity =>
        ////////////////////////////////////////////////////////////////////////
        // Create declarations for fresh ports
        ////////////////////////////////////////////////////////////////////////
        if (freshIPortSymbols.top.isEmpty && freshOPortSymbols.top.isEmpty) {
          entity
        } else {
          val freshIPortDecls = for (symbol <- freshIPortSymbols.top.values) yield {
            Decl(Sym(symbol), symbol.denot.kind, None) regularize symbol.loc
          }
          val freshOPortDecls = for (symbol <- freshOPortSymbols.top.values) yield {
            Decl(Sym(symbol), symbol.denot.kind, None) regularize symbol.loc
          }

          val newDecls = freshIPortDecls ++ freshOPortDecls ++ entity.declarations

          // Update type of entity to include new ports
          val Sym(symbol: TypeSymbol) = entity.ref
          val newKind = symbol.denot.kind match {
            case kind: TypeEntity => {
              val newPortSymbols = {
                freshIPortSymbols.top.values ++ freshOPortSymbols.top.values ++ kind.portSymbols
              }
              kind.copy(portSymbols = newPortSymbols.toList)
            }
            case _ => unreachable
          }
          symbol withDenot symbol.denot.copy(kind = newKind)

          entity.copy(declarations = newDecls.toList) withLoc entity.loc withVariant entity.variant
        }
      } valueMap { entity =>
        ////////////////////////////////////////////////////////////////////////
        // Connect fresh inner ports to outer port
        ////////////////////////////////////////////////////////////////////////
        if (freshIConnSymbols.top.isEmpty && freshOConnSymbols.top.isEmpty) {
          entity
        } else {
          def instanceSymbolsOfTypes(symbols: Set[TypeSymbol]): List[TermSymbol] = {
            for {
              instance <- entity.instances
              if symbols contains instance.module.asInstanceOf[Sym].symbol.asInstanceOf[TypeSymbol]
            } yield {
              val Sym(termSymbol: TermSymbol) = instance.ref
              termSymbol
            }
          }

          freshIConnSymbols.top

          val freshIConns = for {
            (srcTermSym, group) <- freshIConnSymbols.top groupBy { _._1 }
          } yield {
            val dstTypeSyms = group map { _._2 }
            val dstInstances = instanceSymbolsOfTypes(dstTypeSyms)
            // TODO: Check multi conn is allowed for flow control type
            val lhs = ExprRef(Sym(srcTermSym))
            val rhss = for (dstInstSym <- dstInstances) yield {
              ExprSelect(ExprRef(Sym(dstInstSym)), srcTermSym.denot.name.str)
            }
            Connect(lhs, rhss) regularize entity.loc
          }

          val freshOConns = for {
            (dstTermSym, group) <- freshOConnSymbols.top groupBy { _._2 }
          } yield {
            val srcTypeSyms = group map { _._1 }
            val srcInstances = instanceSymbolsOfTypes(srcTypeSyms)
            // TODO: Check there is only 1 such instance properly
            assert(srcInstances.length == 1)
            val lhs = ExprSelect(ExprRef(Sym(srcInstances.head)), dstTermSym.denot.name.str)
            val rhss = List(ExprRef(Sym(dstTermSym)))
            Connect(lhs, rhss) regularize entity.loc
          }

          val newConns = freshIConns ++ freshOConns ++ entity.connects

          entity.copy(connects = newConns.toList) withLoc entity.loc withVariant entity.variant
        }
      } valueMap { entity =>
        ////////////////////////////////////////////////////////////////////////
        // Extract the nested entities to the same level as the parent entity
        ////////////////////////////////////////////////////////////////////////
        if (entity.entities.isEmpty) {
          entity
        } else {
          val children = entity.entities
          val parent = entity.copy(entities = Nil) withLoc entity.loc withVariant entity.variant

          val Sym(parentSymbol: TypeSymbol) = entity.ref
          val parentName = parentSymbol.denot.name.str

          // Prefix child names with parent name
          for (child <- children) {
            val Sym(childSymbol: TypeSymbol) = child.ref
            val childName = childSymbol.denot.name.str
            val newName = TypeName(parentName + "__" + childName)
            childSymbol withDenot childSymbol.denot.copy(name = newName)
          }

          Thicket(parent :: children) withLoc entity.loc
        }
      }
    } followedBy {
      freshIConnSymbols.pop()
      freshOConnSymbols.pop()

      // Add ports created in this entity to connections required in the outer entity
      val iConns = for {
        iPortSymbol <- freshIPortSymbols.top.keys
      } yield {
        val Sym(typeSymbol: TypeSymbol) = entity.ref
        (iPortSymbol, typeSymbol)
      }
      val iTop = freshIConnSymbols.top
      freshIConnSymbols.pop().push(iTop | iConns.toSet)

      val oConns = for {
        oPortSymbol <- freshOPortSymbols.top.keys
      } yield {
        val Sym(typeSymbol: TypeSymbol) = entity.ref
        (typeSymbol, oPortSymbol)
      }
      val oTop = freshOConnSymbols.top
      freshOConnSymbols.pop().push(oTop | oConns.toSet)

      freshIPortSymbols.pop()
      freshOPortSymbols.pop()
      outerIPortSymbols.pop()
      outerOPortSymbols.pop()
    }

    // Rewrite references to outer input ports as references to the newly created inner ports
    case node @ ExprRef(Sym(symbol: TermSymbol)) if freshIPortSymbols.top contains symbol => {
      ExprRef(Sym(freshIPortSymbols.top(symbol))) regularize node.loc
    }

    // Rewrite references to outer output ports as references to the newly created inner ports
    case node @ ExprRef(Sym(symbol: TermSymbol)) if freshOPortSymbols.top contains symbol => {
      ExprRef(Sym(freshOPortSymbols.top(symbol))) regularize node.loc
    }

    // Nodes who have rewritten children need types
    case node if !node.hasTpe => TypeAssigner(node)

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(outerIPortSymbols.depth == 1)
    assert(outerOPortSymbols.depth == 1)
    assert(freshIPortSymbols.isEmpty)
    assert(freshOPortSymbols.isEmpty)
    assert(freshIConnSymbols.depth == 1)
    assert(freshOConnSymbols.depth == 1)

    tree visit {
      case node: Entity if node.entities.nonEmpty => {
        cc.ice(node, s"Nested entities remain after LiftEntities")
      }
    }
  }

}
