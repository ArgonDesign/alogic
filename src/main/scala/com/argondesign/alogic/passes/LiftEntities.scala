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

import scala.collection.mutable

final class LiftEntities(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy
    with ValueMap {

  // TODO: Only works for single nesting
  // TODO: Rewrite without collectAll
  // TODO: Pass down consts

  // ports declared in outer entities
  val outerIPortSymbols: Stack[Set[TermSymbol]] = Stack()
  val outerOPortSymbols: Stack[Set[TermSymbol]] = Stack()

  // new ports that need to be created to connect up to directly accessed outer port
  val freshIPortSymbols: Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]] = Stack()
  val freshOPortSymbols: Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]] = Stack()

  // new ports that need to be connected in this entity
  val freshIConnSymbols: Stack[mutable.LinkedHashSet[(TermSymbol, TypeSymbol)]] = Stack()
  val freshOConnSymbols: Stack[mutable.LinkedHashSet[(TypeSymbol, TermSymbol)]] = Stack()

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      //////////////////////////////////////////////////////////////////////////
      // Collect outer ports we are referencing
      //////////////////////////////////////////////////////////////////////////

      val referencedSymbols = {
        val it = entity collectAll {
          case ExprRef(Sym(symbol: TermSymbol)) => symbol
        }
        it.toList
      }

      val newIPortSymbols = for {
        outerSymbol <- referencedSymbols
        if outerIPortSymbols.toList.exists(_ contains outerSymbol)
      } yield {
        val innerSymbol = cc.newSymbolLike(outerSymbol)
        outerSymbol -> innerSymbol
      }
      freshIPortSymbols.push(mutable.LinkedHashMap(newIPortSymbols: _*))

      val newOPortSymbols = for {
        outerSymbol <- referencedSymbols
        if outerOPortSymbols.toList.exists(_ contains outerSymbol)
      } yield {
        val innerSymbol = cc.newSymbolLike(outerSymbol)
        outerSymbol -> innerSymbol
      }
      freshOPortSymbols.push(mutable.LinkedHashMap(newOPortSymbols: _*))

      //////////////////////////////////////////////////////////////////////////
      // Push ports declared by us
      //////////////////////////////////////////////////////////////////////////

      val newISymbols = entity.declarations collect {
        case Decl(Sym(symbol: TermSymbol), _: TypeIn, _) => symbol
      }
      outerIPortSymbols.push(newISymbols.toSet)

      val newOSymbols = entity.declarations collect {
        case Decl(Sym(symbol: TermSymbol), _: TypeOut, _) => symbol
      }
      outerOPortSymbols.push(newOSymbols.toSet)

      //////////////////////////////////////////////////////////////////////////
      // Push placeholder empty map for fresh connections
      //////////////////////////////////////////////////////////////////////////

      freshIConnSymbols.push(mutable.LinkedHashSet())
      freshOConnSymbols.push(mutable.LinkedHashSet())
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

          val newEntity = entity.copy(
            declarations = newDecls.toList
          ) withLoc entity.loc withVariant entity.variant
          TypeAssigner(newEntity)
        }
      } valueMap { entity =>
        ////////////////////////////////////////////////////////////////////////
        // Connect fresh inner ports to outer port
        ////////////////////////////////////////////////////////////////////////
        if (freshIConnSymbols.top.isEmpty && freshOConnSymbols.top.isEmpty) {
          entity
        } else {
          def instanceSymbolsOfType(eSymbol: TypeSymbol): List[TermSymbol] = {
            entity.instances collect {
              case Instance(Sym(iSymbol: TermSymbol), Sym(`eSymbol`), _, _) => iSymbol
            }
          }

          val freshIConns = for {
            (srcPortSymbol, dstEntitySymbol) <- freshIConnSymbols.top
            dstInstanceSymbol <- instanceSymbolsOfType(dstEntitySymbol)
          } yield {
            val lhs = ExprRef(Sym(srcPortSymbol))
            val rhs = ExprSelect(ExprRef(Sym(dstInstanceSymbol)), srcPortSymbol.name)
            Connect(lhs, List(rhs)) regularize entity.loc
          }

          val freshOConns = for {
            (srcEntitySymbol, dstPortSymbol) <- freshOConnSymbols.top
            srcInstanceSymbol <- instanceSymbolsOfType(srcEntitySymbol)
          } yield {
            val lhs = ExprSelect(ExprRef(Sym(srcInstanceSymbol)), dstPortSymbol.name)
            val rhs = ExprRef(Sym(dstPortSymbol))
            Connect(lhs, List(rhs)) regularize entity.loc
          }

          val newEntity = entity.copy(
            connects = entity.connects ++ freshIConns ++ freshOConns
          ) withLoc entity.loc withVariant entity.variant
          TypeAssigner(newEntity)
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
          TypeAssigner(parent)

          val Sym(parentSymbol: TypeSymbol) = entity.ref
          val parentName = parentSymbol.name

          // Prefix child names with parent name
          for (child <- children) {
            val Sym(childSymbol: TypeSymbol) = child.ref
            val childName = childSymbol.name
            val newName = TypeName(parentName + "__" + childName)
            childSymbol withDenot childSymbol.denot.copy(name = newName)
          }

          TypeAssigner(Thicket(parent :: children) withLoc entity.loc)
        }
      }
    } followedBy {
      freshIConnSymbols.pop()
      freshOConnSymbols.pop()

      // Add ports created in this entity to connections required in the outer entity
      if (freshIConnSymbols.nonEmpty) {
        val iConns = for {
          (iPortSymbol, _) <- freshIPortSymbols.top
        } yield {
          val Sym(typeSymbol: TypeSymbol) = entity.ref
          (iPortSymbol, typeSymbol)
        }
        freshIConnSymbols.top ++= iConns
      }

      if (freshOConnSymbols.nonEmpty) {
        val oConns = for {
          (oPortSymbol, _) <- freshOPortSymbols.top
        } yield {
          val Sym(typeSymbol: TypeSymbol) = entity.ref
          (typeSymbol, oPortSymbol)
        }
        freshOConnSymbols.top ++= oConns
      }

      freshIPortSymbols.pop()
      freshOPortSymbols.pop()
      outerIPortSymbols.pop()
      outerOPortSymbols.pop()
    }

    // Rewrite references to outer ports as references to the newly created inner ports
    case ExprRef(Sym(symbol: TermSymbol)) => {
      (freshIPortSymbols.top.get(symbol) orElse freshOPortSymbols.top.get(symbol)) map {
        innerSymbol =>
          ExprRef(Sym(innerSymbol)) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(outerIPortSymbols.isEmpty)
    assert(outerOPortSymbols.isEmpty)
    assert(freshIPortSymbols.isEmpty)
    assert(freshOPortSymbols.isEmpty)
    assert(freshIConnSymbols.isEmpty)
    assert(freshOConnSymbols.isEmpty)

    tree visit {
      case node: Entity if node.entities.nonEmpty => {
        cc.ice(node, s"Nested entities remain after LiftEntities")
      }
    }
  }

}
