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
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.transform.ReplaceTermRefs
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

  // ports and consts declared in outer entities
  private val outerIPortSymbols: Stack[Set[TermSymbol]] = Stack()
  private val outerOPortSymbols: Stack[Set[TermSymbol]] = Stack()
  private val outerConstSymbols: Stack[Set[TermSymbol]] = Stack()

  // new ports that need to be created to connect up to directly accessed outer port
  private val freshIPortSymbols: Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]] = Stack()
  private val freshOPortSymbols: Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]] = Stack()
  // new costs that need to be created
  private val freshConstSymbols: Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]] = Stack()

  // new ports that need to be connected in this entity
  private val freshIConnSymbols: Stack[mutable.LinkedHashSet[(TermSymbol, TypeSymbol)]] = Stack()
  private val freshOConnSymbols: Stack[mutable.LinkedHashSet[(TypeSymbol, TermSymbol)]] = Stack()

  // Output ports with storage that have been pushed into nested entities need
  // to loose their storage and turn into wire ports, we collect these in a set
  private val stripStorageSymbols = mutable.Set[TermSymbol]()

  private var entityCount = 0

  override def skip(tree: Tree): Boolean = tree match {
    // Skip root entities without any nested entities
    case entity: Entity => {
      entityCount == 0 && entity.entities.isEmpty
    } followedBy {
      entityCount += 1
    }
    case _ => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      //////////////////////////////////////////////////////////////////////////
      // Collect outer ports and consts we are referencing
      //////////////////////////////////////////////////////////////////////////

      lazy val referencedSymbols = {
        val it = entity collectAll {
          case ExprRef(symbol: TermSymbol) => symbol
        }
        it.toList
      }

      val newIPortSymbols = if (outerIPortSymbols.isEmpty) {
        Nil
      } else {
        for {
          outerSymbol <- referencedSymbols
          if outerIPortSymbols.toList.exists(_ contains outerSymbol)
        } yield {
          val innerSymbol = cc.newSymbolLike(outerSymbol)
          outerSymbol -> innerSymbol
        }
      }
      freshIPortSymbols.push(mutable.LinkedHashMap(newIPortSymbols: _*))

      val newOPortSymbols = if (outerOPortSymbols.isEmpty) {
        Nil
      } else {
        for {
          outerSymbol <- referencedSymbols
          if outerOPortSymbols.toList.exists(_ contains outerSymbol)
        } yield {
          val innerSymbol = cc.newSymbolLike(outerSymbol)
          outerSymbol -> innerSymbol
        }
      }
      freshOPortSymbols.push(mutable.LinkedHashMap(newOPortSymbols: _*))

      val newConstSymbols = if (outerConstSymbols.isEmpty) {
        Nil
      } else {
        val referenced = for {
          outerSymbol <- referencedSymbols
          if outerConstSymbols.toList.exists(_ contains outerSymbol)
        } yield {
          outerSymbol
        }
        val initializer = referenced flatMap { outerSymbol =>
          outerSymbol.attr.constValue.value collect {
            case ExprRef(s: TermSymbol) if outerConstSymbols.toList.exists(_ contains s) => s
          }
        }
        (referenced ::: initializer).distinct map { outerSymbol =>
          outerSymbol -> cc.newSymbolLike(outerSymbol)
        }
      }
      freshConstSymbols.push(mutable.LinkedHashMap(newConstSymbols: _*))

      //////////////////////////////////////////////////////////////////////////
      // Update the constValue attributes of the new symbols
      //////////////////////////////////////////////////////////////////////////

      lazy val rewrite: Expr => Expr = {
        val bindings = freshConstSymbols.top mapValues { innerSymbol =>
          ExprRef(innerSymbol) regularize innerSymbol.loc
        }
        val tt = new ReplaceTermRefs(bindings)
        tt(_).asInstanceOf[Expr]
      }
      for (innerSymbol <- freshConstSymbols.top.values) {
        innerSymbol.attr.constValue set rewrite(innerSymbol.attr.constValue.value)
      }

      //////////////////////////////////////////////////////////////////////////
      // Mark output ports to strip storage from
      //////////////////////////////////////////////////////////////////////////

      for ((outerSymbol, _) <- newOPortSymbols) {
        stripStorageSymbols add outerSymbol
      }

      //////////////////////////////////////////////////////////////////////////
      // Push ports and consts declared by us
      //////////////////////////////////////////////////////////////////////////

      val newISymbols = entity.declarations collect {
        case Decl(symbol, _) if symbol.kind.isIn => symbol
      }
      outerIPortSymbols.push(newISymbols.toSet)

      val newOSymbols = entity.declarations collect {
        case Decl(symbol, _) if symbol.kind.isOut => symbol
      }
      outerOPortSymbols.push(newOSymbols.toSet)

      val newCSymbols = entity.declarations collect {
        case Decl(symbol, _) if symbol.kind.isConst => symbol
      }
      outerConstSymbols.push(newCSymbols.toSet)

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
            Decl(symbol, None) regularize symbol.loc
          }
          val freshOPortDecls = for (symbol <- freshOPortSymbols.top.values) yield {
            Decl(symbol, None) regularize symbol.loc
          }

          val newDecls = freshIPortDecls ++ freshOPortDecls ++ entity.declarations

          // Update type of entity to include new ports
          val Sym(symbol: TypeSymbol) = entity.ref
          val newKind = symbol.kind match {
            case kind: TypeEntity => {
              val newPortSymbols = {
                freshIPortSymbols.top.values ++ freshOPortSymbols.top.values ++ kind.portSymbols
              }
              kind.copy(portSymbols = newPortSymbols.toList)
            }
            case _ => unreachable
          }
          symbol.kind = newKind

          TypeAssigner {
            entity.copy(
              declarations = newDecls.toList
            ) withLoc entity.loc withVariant entity.variant
          }
        }
      } valueMap { entity =>
        ////////////////////////////////////////////////////////////////////////
        // Create declarations for fresh consts
        ////////////////////////////////////////////////////////////////////////
        if (freshConstSymbols.top.isEmpty) {
          entity
        } else {
          val freshConstDecls = for (symbol <- freshConstSymbols.top.values) yield {
            Decl(symbol, symbol.attr.constValue.get) regularize symbol.loc
          }

          val newDecls = freshConstDecls ++ entity.declarations

          TypeAssigner {
            entity.copy(
              declarations = newDecls.toList
            ) withLoc entity.loc withVariant entity.variant
          }
        }
      } valueMap { entity =>
        ////////////////////////////////////////////////////////////////////////
        // Strip storage from output ports where needed
        ////////////////////////////////////////////////////////////////////////
        if (stripStorageSymbols.nonEmpty) {
          entity.declarations foreach {
            case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeOut] => {
              val TypeOut(kind, fc, st) = symbol.kind
              if (st != StorageTypeDefault && (stripStorageSymbols contains symbol)) {
                val newKind = TypeOut(kind, fc, StorageTypeDefault)
                symbol.kind = newKind
              }
            }
            case _ => ()
          }
        }
        entity
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
            val lhs = ExprRef(srcPortSymbol)
            val rhs = ExprSelect(ExprRef(dstInstanceSymbol), srcPortSymbol.name)
            Connect(lhs, List(rhs)) regularize entity.loc
          }

          val freshOConns = for {
            (srcEntitySymbol, dstPortSymbol) <- freshOConnSymbols.top
            srcInstanceSymbol <- instanceSymbolsOfType(srcEntitySymbol)
          } yield {
            val lhs = ExprSelect(ExprRef(srcInstanceSymbol), dstPortSymbol.name)
            val rhs = ExprRef(dstPortSymbol)
            Connect(lhs, List(rhs)) regularize entity.loc
          }

          TypeAssigner {
            entity.copy(
              connects = entity.connects ++ freshIConns ++ freshOConns
            ) withLoc entity.loc withVariant entity.variant
          }
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
            childSymbol rename (parentName + cc.sep + childName)
          }

          TypeAssigner(Thicket(parent :: children) withLoc entity.loc)
        }
      }
    } followedBy {
      freshIConnSymbols.pop()
      freshOConnSymbols.pop()
      freshConstSymbols.pop()

      // Add ports created in this entity to connections required in the outer entity
      if (freshIConnSymbols.nonEmpty) {
        for ((iPortSymbol, _) <- freshIPortSymbols.top) {
          val Sym(typeSymbol: TypeSymbol) = entity.ref
          freshIConnSymbols.top.add((iPortSymbol, typeSymbol))
        }
      }

      if (freshOConnSymbols.nonEmpty) {
        for ((oPortSymbol, _) <- freshOPortSymbols.top) {
          val Sym(typeSymbol: TypeSymbol) = entity.ref
          freshOConnSymbols.top.add((typeSymbol, oPortSymbol))
        }
      }

      freshIPortSymbols.pop()
      freshOPortSymbols.pop()
      outerIPortSymbols.pop()
      outerOPortSymbols.pop()
      outerConstSymbols.pop()
    }

    // Rewrite references to outer ports as references to the newly created inner ports
    case ExprRef(symbol: TermSymbol) => {
      freshIPortSymbols.top.get(symbol) orElse
        freshOPortSymbols.top.get(symbol) orElse
        freshConstSymbols.top.get(symbol) map { innerSymbol =>
        ExprRef(innerSymbol) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(outerIPortSymbols.isEmpty)
    assert(outerOPortSymbols.isEmpty)
    assert(outerConstSymbols.isEmpty)
    assert(freshIPortSymbols.isEmpty)
    assert(freshOPortSymbols.isEmpty)
    assert(freshConstSymbols.isEmpty)
    assert(freshIConnSymbols.isEmpty)
    assert(freshOConnSymbols.isEmpty)

    tree visit {
      case node: Entity if node.entities.nonEmpty => {
        cc.ice(node, s"Nested entities remain after LiftEntities")
      }
    }
  }

}

object LiftEntities extends TreeTransformerPass {
  val name = "lift-entities"
  def create(implicit cc: CompilerContext) = new LiftEntities
}
