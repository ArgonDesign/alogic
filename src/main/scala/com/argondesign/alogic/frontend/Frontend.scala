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
// The highest level interface around the compiler frontend.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import java.io.File

import com.argondesign.alogic.FindFile
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration.Inf

// The frontend is responsible for:
// - locating sources (name -> source)
// - preprocessing (source -> source)
// - parsing (source -> ast)

class Frontend(
    val moduleSeachDirs: List[File],
    val includeSeachDirs: List[File],
    val initialDefines: Map[String, String]
)(
    implicit
    cc: CompilerContext
) {

  private[this] def locateIt(entityName: String): Source = {
    val sourceFileOpt = FindFile(entityName + ".alogic", moduleSeachDirs, maxDepth = 1)

    val sourceFile = sourceFileOpt.getOrElse {
      cc.fatal(s"Cannot find entity '${entityName}'. Looked in:" :: moduleSeachDirs map {
        _.toString
      }: _*)
    }

    Source(sourceFile)
  }

  private[this] def preprocessIt(source: Source): Source = {
    val preprocessor = new Preprocessor
    preprocessor(source, initialDefines, includeSeachDirs)
  }

  private[this] def parseIt(source: Source, fileName: String): Root = {
    Parser[Root](source).getOrElse {
      cc.fatal("Stopping due to syntax errors")
    }
  }

  // Cache of trees we already started working on. We use this to to avoid
  // repeated processing of entities that are instantiated multiple times
  val inProgress = mutable.Map[String, Future[Set[Root]]]()

  // Recursive worker that builds a future yielding the set of all trees required for an entity
  def doIt(entityName: String): Future[Set[Root]] = {

    // The actual future that builds all parse trees under the hierarchy of the given entity
    // note that this is only actually constructed if the inProgress map does not contain the
    // future already
    lazy val future = {
      // Fetch source
      val sourceFuture = Future { locateIt(entityName) }

      // Preproces it
      val preprocessedFuture = sourceFuture map preprocessIt

      // Parse it
      val astFuture = preprocessedFuture map { source =>
        val root = parseIt(source, entityName)

        // Check that the entityName used for file search matches the actual entity name
        // defined in the top level entity in the file, we rely on this to find the
        // source file of an instance, so there is no way forward if this is violated
        val parsedName = root.entity.name

        if (parsedName != entityName) {
          root.entity match {
            case Entity(ident: Ident, _) =>
              cc.fatal(ident,
                       s"Entity name '${parsedName}' does not match file basename '${entityName}'")
            case _ => unreachable
          }
        }

        root
      }

      // Recursively process all instantiated nodes
      val childAstsFuture = {

        // Extract all instance entity names (from the recursively nested entities as well)
        val instantiatedEntityNamesFuture = astFuture map { root =>
          def loop(entity: Entity): List[String] = {
            val localNames = entity.name :: (entity.entities map { _.name })
            val requiredExternalNames = entity.instances collect {
              case EntInstance(_, Ident(name), _, _) if !(localNames contains name) => name
            }
            requiredExternalNames ::: (entity.entities flatMap loop)
          }

          loop(root.entity)
        }

        // process them extracted names recursively
        instantiatedEntityNamesFuture flatMap { Future.traverse(_)(doIt) }
      }

      // Merge child tree sets and add this tree
      for {
        childAsts <- childAstsFuture
        ast <- astFuture
      } yield {
        childAsts.foldLeft(Set.empty[Root])(_ union _) + ast
      }
    }

    // Now look up the result, or create the future if we have not started it yet
    synchronized {
      inProgress.getOrElseUpdate(entityName, future)
    }
  }

  // Parse all files needed for 'topLevelNames'. Returns list of Root nodes
  def apply(topLevelNames: List[String]): List[Root] = {

    // Process all specified top level entities
    val treeSetFutures = Future.traverse(topLevelNames)(doIt)

    // Merge all sets
    val treeSetFuture = treeSetFutures map { _.foldLeft(Set.empty[Root])(_ union _) }

    // Wait for the result
    val treeSet = Await.result(treeSetFuture, atMost = Inf)

    // Mark top level entities as such
    for {
      Root(_, Entity(ident: Ident, _)) <- treeSet
      if topLevelNames contains ident.name
    } {
      val newAttr = Map("toplevel" -> (Expr(1) withLoc ident.loc))
      ident withAttr {
        if (ident.hasAttr) ident.attr ++ newAttr else newAttr
      }
    }

    // Return the trees
    treeSet.toList
  }
}
