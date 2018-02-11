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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source

import org.antlr.v4.runtime.ParserRuleContext

// The frontend is responsible to preprocess, parse, and build ASTs for all entities including,
// and below the hierarchy of a given toplevel entity.
class Frontend(
  val moduleSeachDirs:  List[File],
  val includeSeachDirs: List[File],
  val initialDefines:   Map[String, String]
)(
  implicit
  cc: CompilerContext
) {

  private[this] def findSource(entityName: String): Source = {
    val sourceFileOpt = FindFile(entityName + ".alogic", moduleSeachDirs, maxDepth = 1)

    val sourceFile = sourceFileOpt.getOrElse {
      cc.fatal(s"Cannot find entity '${entityName}'. Looked in:" :: moduleSeachDirs map { _.toString }: _*)
    }

    Source(sourceFile)
  }

  private[this] def parseSource(source: Source): ParserRuleContext = {
    // Preprocessor
    val preprocessor = new Preprocessor

    val preprocessed = preprocessor(source, initialDefines, includeSeachDirs)

    val preprocessedSource = Source(source.file, preprocessed)

    // Parser
    Parser(preprocessedSource).getOrElse {
      cc.fatal("Stopping due to syntax errors")
    }
  }

  // Parse all files needed for 'entityName'. Returns map from entityNames -> ASTs
  def apply(entityName: String): Map[String, ParserRuleContext] = {
    // Cache of parses we already started workingon. We use this to to avoid
    // multiple parses of files that are instantiated multiple times
    val inProgress = mutable.Map[String, Future[Map[String, ParserRuleContext]]]()

    // Recursive worker that builds a future yielding the map of all parse trees required for an entity
    def parse(entityName: String): Future[Map[String, ParserRuleContext]] = {

      // The actual future that builds all parse trees under the hierarchy of the given entity
      // note that this is only actually constructed if the inProgress map does not contain the
      // future already
      lazy val future = {
        // Fetch source
        val sourceFuture = Future {
          findSource(entityName)
        }

        // Parse it
        val parseTreeFuture = sourceFuture map parseSource

        // Find all instance entity names
        val instantiatedEntityNamesFuture = parseTreeFuture map { InstanceEntityNameExtractor(_) }

        // Get all their parse tree maps recursively
        val childParseTreeMapsFuture = instantiatedEntityNamesFuture flatMap { Future.traverse(_)(parse) }

        // Merge child parse tree maps and add this parse tree
        for {
          childParseTreeMaps <- childParseTreeMapsFuture
          parseTree <- parseTreeFuture
        } yield {
          (Map.empty[String, ParserRuleContext] /: childParseTreeMaps)(_ ++ _) + (entityName -> parseTree)
        }
      }

      // Now look up the result, or create the future if we have not started it yet
      synchronized {
        inProgress.getOrElseUpdate(entityName, future)
      }
    }

    Await.result(parse(entityName), atMost = Inf)
  }

}
