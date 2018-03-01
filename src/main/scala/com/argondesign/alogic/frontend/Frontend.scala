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

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration.Inf

import com.argondesign.alogic.FindFile
import com.argondesign.alogic.antlr.AlogicParser.StartContext
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.ast.Trees.Instance
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.util.unreachable

// The frontend is responsible to:
// - locating sources (name -> source)
// - preprocessing (source -> source)
// - parsing (source -> parse tree)
// - building (parse tree -> ast)
// - naming (resolve identifiers to symbols)
// - typing (compute types of all tree nodes)

class Frontend(
  val moduleSeachDirs:  List[File],
  val includeSeachDirs: List[File],
  val initialDefines:   Map[String, String]
)(
  implicit
  cc: CompilerContext
) {

  private[this] def locateIt(entityName: String): Source = {
    val sourceFileOpt = FindFile(entityName + ".alogic", moduleSeachDirs, maxDepth = 1)

    val sourceFile = sourceFileOpt.getOrElse {
      cc.fatal(s"Cannot find entity '${entityName}'. Looked in:" :: moduleSeachDirs map { _.toString }: _*)
    }

    Source(sourceFile)
  }

  private[this] def preprocessIt(source: Source): Source = {
    val preprocessor = new Preprocessor
    preprocessor(source, initialDefines, includeSeachDirs)
  }

  private[this] def parseIt(source: Source): StartContext = {
    Parser[StartContext](source).getOrElse {
      cc.fatal("Stopping due to syntax errors")
    }
  }

  private[this] def buildIt(entityName: String, parseTree: StartContext): Root = {
    val tree = Builder(parseTree)

    // Check that the entityName used for file search matches the actual name defined
    // in the top level entity in the file
    val Root(_, entity) = tree
    val Ident(name) = entity.ref

    if (name != entityName) {
      cc.fatal(entity.loc, s"File name does not match entity name '${entityName}'")
    }

    tree
  }

  private[this] def nameIt(tree: Root): Entity = {
    tree.rewrite(new Namer) match {
      case entity: Entity => entity
      case _              => unreachable
    }
  }

  private[this] def desugarIt(tree: Entity): Entity = tree

  private[this] def typeIt(tree: Entity): Entity = tree

  // Cache of trees we already started working on. We use this to to avoid
  // multiple processing files that are instantiated multiple times
  val inProgress = mutable.Map[String, Future[Map[String, Root]]]()

  // Recursive worker that builds a future yielding the map of all trees required for an entity
  def doIt(entityName: String): Future[Map[String, Root]] = {

    // The actual future that builds all parse trees under the hierarchy of the given entity
    // note that this is only actually constructed if the inProgress map does not contain the
    // future already
    lazy val future = {
      // Fetch source
      val sourceFuture = Future { locateIt(entityName) }

      // Preproces it
      val preprocessedFuture = sourceFuture map preprocessIt

      // Parse it
      val parseTreeFuture = preprocessedFuture map parseIt

      // Build it
      val astFuture = parseTreeFuture map { buildIt(entityName, _) }

      // Recursively process all instantiated nodes
      val childAstMapsFuture = {

        // Extract all instance entity names (from the recursively nested entities as well)
        val instantiatedEntityNamesFuture = astFuture map { root =>
          def loop(entity: Entity): List[String] = {
            val nestedEntities = entity.entities
            val nestedNames = nestedEntities map {
              _.ref match {
                case Ident(name) => name
                case _           => unreachable
              }
            }
            val requiredExternalNames = entity.instances collect {
              case Instance(_, Ident(name), _, _) if !(nestedNames contains name) => name
            }
            requiredExternalNames ::: (nestedEntities flatMap loop)
          }

          loop(root.entity)
        }

        // process them extracted names recursively
        instantiatedEntityNamesFuture flatMap { Future.traverse(_)(doIt) }
      }

      // Merge child tree maps and add this  tree
      for {
        childAstMaps <- childAstMapsFuture
        ast <- astFuture
      } yield {
        (Map.empty[String, Root] /: childAstMaps)(_ ++ _) + (entityName -> ast)
      }
    }

    // Now look up the result, or create the future if we have not started it yet
    synchronized {
      inProgress.getOrElseUpdate(entityName, future)
    }
  }

  // Parse all files needed for 'entityName'. Returns map from entityNames -> Root
  def apply(entityName: String): Map[String, Entity] = {

    // Compute the result
    val initialMap = Await.result(doIt(entityName), atMost = Inf)

    // Insert entity symbols into the global scope
    val globalEntities = initialMap.values map {
      case Root(_, entity) => entity
      case _               => unreachable
    }
    cc.addGlobalEntities(globalEntities)

    val astMap = initialMap.par mapValues {
      nameIt(_)
    } mapValues {
      desugarIt(_)
    } mapValues {
      typeIt(_)
    }

    // Return the AST map
    astMap.seq.toMap
  }

}
