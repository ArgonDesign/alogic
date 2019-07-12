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
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class LiftEntities(implicit cc: CompilerContext) extends TreeTransformer {

  // TODO: Only works for single nesting
  // TODO: Rewrite without collectAll

  // ports and consts declared in outer entities
  private val outerIPortSymbols = mutable.Stack[Set[TermSymbol]]()
  private val outerOPortSymbols = mutable.Stack[Set[TermSymbol]]()
  private val outerConstSymbols = mutable.Stack[Set[TermSymbol]]()

  // new ports that need to be created to connect up to directly accessed outer port
  private val freshIPortSymbols = mutable.Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]]()
  private val freshOPortSymbols = mutable.Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]]()
  // new costs that need to be created
  private val freshConstSymbols = mutable.Stack[mutable.LinkedHashMap[TermSymbol, TermSymbol]]()

  // new ports that need to be connected in this entity
  private val freshIConnSymbols = mutable.Stack[mutable.LinkedHashSet[(TermSymbol, TypeSymbol)]]()
  private val freshOConnSymbols = mutable.Stack[mutable.LinkedHashSet[(TypeSymbol, TermSymbol)]]()

  // Output ports with storage that have been pushed into nested entities need
  // to loose their storage and turn into wire ports, we collect these in a set
  private val stripStorageSymbols = mutable.Set[TermSymbol]()

  // Output slices are pushed into the referencing entity. We keep track of
  // referencing entities, and error if there is more than one, as this would
  // result in multiple slice instances driving the output ports.
  // TODO: This could be relaxed by allowing more than one nested entities to
  // reference an outer port so long as there is only one doing a .write or
  // assignment. The rest could have the referenced signals wired through to
  // them.
  private val outerORefs = mutable.Map[String, TermSymbol]()

  // Similarly to output slices, it is also invalid to reference an input
  // port with sync ready flow control from more than one nested entities, so
  // we keep track of those as well
  private val outerIRefs = mutable.Map[String, TermSymbol]()

  private var nestingLevel = 0

  override def skip(tree: Tree): Boolean = tree match {
    // Skip root entities without any nested entities
    case entity: Entity => nestingLevel == 0 && entity.entities.isEmpty
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      nestingLevel += 1

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
        // Find all referenced constants
        val referenced = for {
          outerSymbol <- referencedSymbols
          if outerConstSymbols.toList.exists(_ contains outerSymbol)
        } yield {
          outerSymbol
        }

        // Recursively find all constants used in initializers of referenced constants
        @tailrec
        def loop(prev: List[TermSymbol], curr: List[TermSymbol]): List[TermSymbol] = {
          if (prev == curr) {
            curr
          } else {
            val referenced = curr flatMap { outerSymbol =>
              outerSymbol.attr.init.value collect {
                case ExprRef(s: TermSymbol) if outerConstSymbols.toList.exists(_ contains s) => s
              }
            }
            loop(curr, (curr ::: referenced).distinct)
          }
        }

        // Sort the symbols in source order and create new symbols
        loop(Nil, referenced) sortBy { _.loc.start } map { outerSymbol =>
          outerSymbol -> cc.newSymbolLike(outerSymbol)
        }
      }
      freshConstSymbols.push(mutable.LinkedHashMap(newConstSymbols: _*))

      //////////////////////////////////////////////////////////////////////////
      // Update the init attributes of the new symbols
      //////////////////////////////////////////////////////////////////////////

      lazy val rewrite: Expr => Expr = {
        val bindings = freshConstSymbols.top.view mapValues { innerSymbol =>
          ExprRef(innerSymbol) regularize innerSymbol.loc
        }
        _ given bindings.toMap
      }
      for (innerSymbol <- freshConstSymbols.top.values) {
        innerSymbol.attr.init set rewrite(innerSymbol.attr.init.value)
      }

      //////////////////////////////////////////////////////////////////////////
      // Mark output ports to strip storage from
      //////////////////////////////////////////////////////////////////////////

      for ((outerSymbol, _) <- newOPortSymbols) {
        stripStorageSymbols add outerSymbol
      }

      //////////////////////////////////////////////////////////////////////////
      // Record references
      //////////////////////////////////////////////////////////////////////////

      for ((outerSymbol, _) <- newOPortSymbols) {
        outerORefs(entity.symbol.name) = outerSymbol
      }

      for ((outerSymbol, _) <- newIPortSymbols) {
        outerSymbol.kind match {
          case TypeIn(_, FlowControlTypeReady) => outerIRefs(entity.symbol.name) = outerSymbol
          case _                               =>
        }
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

  def finishEntity(entity: Entity): List[Entity] =
    entity pipe { entity =>
      ////////////////////////////////////////////////////////////////////////
      // Create declarations for fresh ports
      ////////////////////////////////////////////////////////////////////////
      if (freshIPortSymbols.top.isEmpty && freshOPortSymbols.top.isEmpty) {
        entity
      } else {
        val freshIPortDecls = for (symbol <- freshIPortSymbols.top.values) yield {
          EntDecl(Decl(symbol, None)) regularize symbol.loc
        }
        val freshOPortDecls = for (symbol <- freshOPortSymbols.top.values) yield {
          EntDecl(Decl(symbol, None)) regularize symbol.loc
        }

        // Update type of entity to include new ports
        val newKind = entity.symbol.kind match {
          case kind: TypeEntity => {
            val newPortSymbols = {
              freshIPortSymbols.top.values ++ freshOPortSymbols.top.values ++ kind.portSymbols
            }
            kind.copy(portSymbols = newPortSymbols.toList)
          }
          case _ => unreachable
        }
        entity.symbol.kind = newKind

        TypeAssigner {
          entity.copy(
            body = List.concat(freshIPortDecls, freshOPortDecls, entity.body)
          ) withLoc entity.loc
        }
      }
    } pipe { entity =>
      ////////////////////////////////////////////////////////////////////////
      // Create declarations for fresh consts
      ////////////////////////////////////////////////////////////////////////
      if (freshConstSymbols.top.isEmpty) {
        entity
      } else {
        val freshConstDecls = for (symbol <- freshConstSymbols.top.values) yield {
          EntDecl(Decl(symbol, symbol.attr.init.get)) regularize symbol.loc
        }

        TypeAssigner {
          entity.copy(
            body = List.concat(freshConstDecls, entity.body)
          ) withLoc entity.loc
        }
      }
    } pipe { entity =>
      ////////////////////////////////////////////////////////////////////////
      // Strip storage from output ports where needed
      ////////////////////////////////////////////////////////////////////////
      if (stripStorageSymbols.nonEmpty) {
        entity.declarations foreach {
          case Decl(symbol, _) =>
            symbol.kind match {
              case TypeOut(_, _, StorageTypeDefault) =>
              case kind: TypeOut if stripStorageSymbols contains symbol =>
                symbol.kind = kind.copy(st = StorageTypeDefault)
              case _ =>
            }
          case _ =>
        }
      }
      entity
    } pipe { entity =>
      ////////////////////////////////////////////////////////////////////////
      // Connect fresh inner ports to outer port
      ////////////////////////////////////////////////////////////////////////
      if (freshIConnSymbols.top.isEmpty && freshOConnSymbols.top.isEmpty) {
        entity
      } else {
        def instanceSymbolsOfType(eSymbol: TypeSymbol): List[TermSymbol] = {
          entity.instances collect {
            case EntInstance(Sym(iSymbol: TermSymbol), Sym(`eSymbol`), _, _) => iSymbol
          }
        }

        val freshIConns = for {
          (srcPortSymbol, dstEntitySymbol) <- freshIConnSymbols.top
          dstInstanceSymbol <- instanceSymbolsOfType(dstEntitySymbol)
        } yield {
          val lhs = ExprRef(srcPortSymbol)
          val rhs = ExprSelect(ExprRef(dstInstanceSymbol), srcPortSymbol.name)
          EntConnect(lhs, List(rhs)) regularize entity.loc
        }

        val freshOConns = for {
          (srcEntitySymbol, dstPortSymbol) <- freshOConnSymbols.top
          srcInstanceSymbol <- instanceSymbolsOfType(srcEntitySymbol)
        } yield {
          val lhs = ExprSelect(ExprRef(srcInstanceSymbol), dstPortSymbol.name)
          val rhs = ExprRef(dstPortSymbol)
          EntConnect(lhs, List(rhs)) regularize entity.loc
        }

        TypeAssigner {
          entity.copy(
            body = List.concat(entity.body, freshIConns, freshOConns)
          ) withLoc entity.loc
        }
      }
    } pipe { entity =>
      ////////////////////////////////////////////////////////////////////////
      // Extract the nested entities to the same level as the parent entity
      ////////////////////////////////////////////////////////////////////////
      if (entity.entities.isEmpty) {
        entity :: Nil
      } else {
        val children = entity.entities
        val parent = {
          val newBody = entity.body filter {
            case _: EntEntity => false
            case _            => true
          }
          TypeAssigner(entity.copy(body = newBody) withLoc entity.loc)
        }

        val parentName = entity.symbol.name

        // Prefix child names with parent name
        for (child <- children) {
          child.symbol rename (parentName + cc.sep + child.symbol.name)
        }

        parent :: children
      }
    } tap { _ =>
      freshIConnSymbols.pop()
      freshOConnSymbols.pop()
      freshConstSymbols.pop()

      // Add ports created in this entity to connections required in the outer entity
      if (freshIConnSymbols.nonEmpty) {
        for ((iPortSymbol, _) <- freshIPortSymbols.top) {
          freshIConnSymbols.top.add((iPortSymbol, entity.symbol))
        }
      }

      if (freshOConnSymbols.nonEmpty) {
        for ((oPortSymbol, _) <- freshOPortSymbols.top) {
          freshOConnSymbols.top.add((entity.symbol, oPortSymbol))
        }
      }

      freshIPortSymbols.pop()
      freshOPortSymbols.pop()
      outerIPortSymbols.pop()
      outerOPortSymbols.pop()
      outerConstSymbols.pop()

      nestingLevel -= 1

      if (nestingLevel == 0) {
        for ((oSymbol, group) <- outerORefs.groupBy { _._2 } if group.size > 1) {
          val first =
            s"Output port '${oSymbol.name}' is referenced by more than one nested entities:"
          cc.error(oSymbol.loc, first :: group.keys.toList: _*)
        }

        for ((iSymbol, group) <- outerIRefs.groupBy { _._2 } if group.size > 1) {
          val first =
            s"Input port '${iSymbol.name}' with 'sync ready' flow control is referenced by more than one nested entities:"
          cc.error(iSymbol.loc, first :: group.keys.toList: _*)
        }
      }
    }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity if nestingLevel == 1 =>
      val results = finishEntity(entity)
      TypeAssigner(Thicket(results) withLoc entity.loc)

    case EntEntity(entity: Entity) =>
      val results = finishEntity(entity) map { entity =>
        TypeAssigner(EntEntity(entity) withLoc entity.loc)
      }
      TypeAssigner(Thicket(results) withLoc entity.loc)

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
