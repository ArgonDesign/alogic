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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.util.unreachable

// The frontend is responsible for:
// - locating sources (name -> source)
// - preprocessing (source -> source)
// - parsing (source -> ast)
// - checking (basic semantic checks on raw ast)
// - naming (resolve identifiers to symbols)
// - desugaring (desugar simple constructs and bring tree to a canonical form)
// - typing (compute types of all tree nodes)

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

  private[this] def checkIt(tree: Root): Root = {
    tree.rewrite(new Checker) match {
      case root: Root => root
      case _          => unreachable
    }
  }

  private[this] def nameIt(tree: Root): Root = {
    tree.rewrite(new Namer) match {
      case root: Root => root
      case _          => unreachable
    }
  }

  private[this] def desugarIt(tree: Root): Root = {
    tree.rewrite(new Desugar) match {
      case root: Root => root
      case _          => unreachable
    }
  }

  private[this] def typeIt(tree: Root): Entity = {
    tree.rewrite(new Typer) match {
      case entity: Entity => entity
      case _              => unreachable
    }
  }

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
      val astFuture = preprocessedFuture map { source =>
        val root = parseIt(source, entityName)

        // Check that the entityName used for file search matches the actual entity name
        // defined in the top level entity in the file, we rely on this to find the
        // source file of an instance, so there is no way forward if this is violated
        val Ident(parsedName) = root.entity.ref

        if (parsedName != entityName) {
          cc.fatal(root.entity.loc,
                   s"Entity name '${parsedName}' does not match file basename '${entityName}'")
        }

        root
      }

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
    cc.addGlobalEntities {
      initialMap.values.seq map {
        case Root(_, entity) => entity
        case _               => unreachable
      }
    }

    // Apply the frontend passes
    val astMap = initialMap mapValues { tree =>
      Future { checkIt(tree) }
    } mapValues {
      _ map { nameIt(_) }
    } mapValues {
      _ map { desugarIt(_) }
    } mapValues {
      _ map { typeIt(_) }
    }

    // Collect results and return the AST map
    astMap mapValues {
      Await.result(_, atMost = Inf)
    }
  }

}
