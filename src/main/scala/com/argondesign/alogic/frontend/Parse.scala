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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.SourceAttribute
import com.argondesign.alogic.passes.Pass

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration.Inf
import scala.util.ChainingSyntax

// The frontend is responsible for:
// - locating sources (name -> source)
// - pre-processing (source -> source)
// - parsing (source -> ast)

class Parse(
    val moduleSeachDirs: List[File],
    val includeSeachDirs: List[File],
    val initialDefines: Map[String, String]
)(
    implicit
    cc: CompilerContext
) extends ChainingSyntax {

  private[this] val rootDescs = mutable.ListBuffer[Desc]()

  private[this] def locateIt(entityName: String, locOpt: Option[Loc]): Source = {
    val sourceFileOpt = FindFile(entityName + ".alogic", moduleSeachDirs, maxDepth = 1)

    val sourceFile = sourceFileOpt.getOrElse {
      val hints = moduleSeachDirs map { _.toString }
      locOpt match {
        case None =>
          cc.fatal(s"Cannot find top level entity '$entityName'. Looked in:" :: hints: _*)
        case Some(loc) =>
          cc.fatal(loc, s"Cannot find entity '$entityName'. Looked in:" :: hints: _*)
      }
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

  private[this] def childNames(trees: List[Tree]): List[String] = trees flatMap {
    case DescEntity(Ident(name, _), _, _) => List(name)
    case EntDesc(desc)                    => childNames(List(desc))
    case EntGen(gen)                      => childNames(List(gen))
    case GenIf(_, thenItems, elseItems)   => childNames(thenItems) ::: childNames(elseItems)
    case GenFor(_, _, _, body)            => childNames(body)
    case GenRange(_, _, _, body)          => childNames(body)
    case _                                => Nil
  }

  // Cache of trees we already started working on. We use this to to avoid
  // repeated processing of entities that are instantiated multiple times
  private[this] val inProgress = mutable.Map[String, Future[Set[Root]]]()

  // Recursive worker that builds a future yielding the set of all trees required for an entity
  private[this] def doIt(name: String, locOpt: Option[Loc]): Future[Set[Root]] = {

    // The actual future that builds all parse trees under the hierarchy of the given entity
    // note that this is only actually constructed if the inProgress map does not contain the
    // future already
    lazy val future = {
      // Fetch source
      val sourceFuture = Future { locateIt(name, locOpt) }

      // Pre-process it
      val preprocessedFuture = sourceFuture map preprocessIt

      // Parse it
      val astFuture = preprocessedFuture map { source =>
        parseIt(source, name) tap { root =>
          // Find the declaration of name, ensure there is one and only one,
          val decl = root.descs filter { _.name == name } match {
            case Nil      => cc.fatal(s"'${source.name}' does not contain the definition of '$name'")
            case d :: Nil => d
            case _        => cc.fatal(s"'${source.name}' contains multiple definitions of '$name'")
          }
          // Add it to the root decls
          synchronized {
            rootDescs append decl
          }
        }
      }

      // Recursively process all instantiated nodes
      val childAstsFuture = {
        // Extract all instance entity names (from the recursively nested entities as well)
        val instantiatedEntityNamesFuture: Future[List[(String, Loc)]] = astFuture map { root =>
          def loop(desc: Desc): List[(String, Loc)] = desc match {
            case d: DescEntity =>
              val localNames = d.name :: childNames(d.body)

              def gatherExternalNames(trees: List[Tree]): List[(String, Loc)] = trees flatMap {
                case d: DescInstance =>
                  d.spec match {
                    case ExprCall(ExprRef(i: Ident), _) if !(localNames contains i.name) =>
                      List((i.name, i.loc))
                    case ExprRef(i: Ident) if !(localNames contains i.name) =>
                      List((i.name, i.loc))
                    case _ => Nil
                  }
                case EntDesc(desc)           => gatherExternalNames(List(desc))
                case EntGen(gen)             => gatherExternalNames(List(gen))
                case GenFor(_, _, _, body)   => gatherExternalNames(body)
                case GenRange(_, _, _, body) => gatherExternalNames(body)
                case GenIf(_, thenItems, elseItems) =>
                  gatherExternalNames(thenItems) ::: gatherExternalNames(elseItems)
                case _ => Nil
              }

              gatherExternalNames(d.body) ::: (d.descs flatMap loop)
            case _ => Nil
          }
          root.descs flatMap loop
        }

        // process the extracted names recursively
        instantiatedEntityNamesFuture flatMap {
          Future.traverse(_) { case (name, loc) => doIt(name, Some(loc)) }
        }
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
      inProgress.getOrElseUpdate(name, future)
    }
  }

  // Parse all files needed for 'topLevelNames'. Returns list of Root nodes
  // and global descs
  def apply(topLevelNames: List[String]): (List[Root], List[Desc]) = {

    // Process all specified top level entities
    val treeSetFutures = Future.traverse(topLevelNames)(doIt(_, None))

    // Merge all sets
    val treeSetFuture = treeSetFutures map { _.foldLeft(Set.empty[Root])(_ union _) }

    // Wait for the result
    val treeSet = Await.result(treeSetFuture, atMost = Inf)

    // Mark top level entities as such
    for {
      Root(body) <- treeSet
      RizDesc(desc) <- body
      if desc.isInstanceOf[DescEntity]
      ident: Ident = desc.ref.asInstanceOf[Ident]
      if topLevelNames contains ident.name
    } {
      ident.attr("toplevel") = SourceAttribute.Flag()
    }

    // Return the trees and global decls
    (treeSet.toList, rootDescs.toList)
  }
}

object Parse extends Pass[List[String], List[Root]] {
  val name = "parser"

  def process(topLevels: List[String])(implicit cc: CompilerContext): List[Root] = {
    val parse = new Parse(cc.settings.moduleSearchDirs,
                          cc.settings.includeSearchDirs,
                          cc.settings.initialDefines)

    // Parse the things
    val (roots, rootDescs) = parse(topLevels)

    // Insert root decls into the global scope
    cc.addGlobalDescs(rootDescs)

    roots
  }

  def dump(result: List[Root], tag: String)(implicit cc: CompilerContext): Unit =
    result foreach { cc.dump(_, "." + tag) }
}
