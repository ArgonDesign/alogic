////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Alogic compiler entry point
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.io.File
import java.io.PrintWriter
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.Sym
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.passes.Passes
import com.argondesign.alogic.util.unreachable

object Main extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Create the compiler context
  //////////////////////////////////////////////////////////////////////////////

  implicit val cc = new CompilerContext

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val cliConf = new CLIConf(args)

  val results = try {
    val toplevel = cliConf.toplevel()

    lazy val cwd = (new File(".")).getCanonicalFile()

    val defaultToCWD = cliConf.ydir.isEmpty && cliConf.incdir.isEmpty && cliConf.srcbase.isEmpty
    val moduleSeachDirs = if (defaultToCWD) List(cwd) else cliConf.ydir()
    val includeSeachDirs = if (defaultToCWD) List(cwd) else cliConf.incdir()

    val initalDefines = cliConf.defs.toMap

    //////////////////////////////////////////////////////////////////////////////
    // Create the front end and built the ASTs
    //////////////////////////////////////////////////////////////////////////////

    val frontEndTrees = {
      val frontend = new Frontend(moduleSeachDirs, includeSeachDirs, initalDefines)
      frontend(toplevel)
    }

    // Insert entity symbols into the global scope
    cc.addGlobalEntities {
      frontEndTrees map {
        case Root(_, entity) => entity
        case _               => unreachable
      }
    }

    val trees = Passes(frontEndTrees)

    Some(trees)
  } catch {
    case _: FatalErrorException => None
  } finally {
    cc.messages map { _.string } foreach Console.err.println
  }

  val oDirBase = cliConf.odir().toPath
  val baseOpt = cliConf.srcbase.toOption map { _.toPath }

  def oPathFor(entity: Entity): Path = {
    val oDir = baseOpt match {
      case None => oDirBase
      case Some(base) => {
        val dirPath = entity.loc.source.file.toPath.toRealPath().getParent
        assert(dirPath startsWith base)
        val relPath = base relativize dirPath
        oDirBase resolve relPath
      }
    }
    val Sym(symbol) = entity.ref
    val name = symbol.denot.name.str + ".v"
    oDir resolve name
  }

  def writeEntity(entity: Entity, oPath: Path): Unit = {
    val oFile = oPath.toFile
    if (!oFile.exists) {
      oFile.getParentFile.mkdirs()
      oFile.createNewFile()
    }
    val pw = new PrintWriter(oFile)
    pw.write(entity.toSource)
    pw.close()
  }

  results match {
    case None => sys exit 1
    case Some(trees) => {
      for (tree <- trees) {
        val entity = tree.asInstanceOf[Entity]
        writeEntity(entity, oPathFor(entity))
      }
      sys exit 0
    }
  }

  //
  //  val cnnf = new CLIConf(args)
  //
  //  // Note that the CLI argument validator checks that the input paths
  //  // and srcdir are consistent, so we can assume they are in the following.
  //  val srcdir: Path = {
  //    conf.srcdir.getOrElse {
  //      conf.path() match {
  //        case path :: Nil if path.isFile => path.parent.get
  //        case path :: Nil                => path
  //        case _                          => unreachable
  //      }
  //    }
  //  }.toRealPath()
  //
  //  val ipaths: List[Path] = {
  //    if (conf.srcdir.isDefined) {
  //      conf.path() map { path => (srcdir / path).toRealPath() }
  //    } else {
  //      conf.path() map { _.toRealPath() }
  //    }
  //  }.distinct
  //
  //  val ifiles: List[Path] = {
  //    ipaths flatMap {
  //      case IsFile(file)     => List(file)
  //      case IsDirectory(dir) => dir.descendants("*.alogic").toList
  //    }
  //  }.distinct
  //
  //  val odir: Path = conf.odir().toRealPath()
  //  if (!odir.exists) {
  //    odir.createDirectory()
  //  }
  //
  //  val multiThreaded = conf.parallel()
  //
  //  //  Message.verbose = conf.verbose()
  //
  //  val includeSearchPaths = conf.incdir()
  //
  //  val initalDefines = conf.defs.toMap
  //
  //  //////////////////////////////////////////////////////////////////////////////
  //  // High level compilation flow
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  def go(implicit cc: CompilerContext): Unit = {
  //
  //    case class Item(task: ast.Task, path: Path)
  //
  //    // Construct potentially parallel file list
  //    val rootPaths = if (multiThreaded) ifiles.par else ifiles
  //
  //    // Build AST
  //    val astItems = {
  //      val rootItems = rootPaths flatMap { path =>
  //        AParser(path, includeSearchPaths, initalDefines) map { Item(_, path) }
  //      }
  //
  //      // Extract embedded FSMs from networks
  //      rootItems flatMap {
  //        case Item(net: ast.NetworkTask, path) => MakeStages(net) match {
  //          case Some((network, stages)) => (network :: stages) map { Item(_, path) }
  //          case None                    => Nil
  //        }
  //        case item => item :: Nil
  //      }
  //    }
  //
  //    // Build catalogue of all modules
  //    // TODO: check for multiple definitions of same module
  //    val moduleCatalogue = {
  //      astItems map { _.task } collect { case t @ ast.Task(_, name, _) => name -> t }
  //    }.toList.toMap
  //
  //    // Synthesise tasks
  //    val taskItems = {
  //      val results = astItems flatMap {
  //        case Item(task: ast.FsmTask, path) => MakeStates(task) map { Item(_, path) }
  //        case item                          => Some(item)
  //      }
  //
  //      // Flatten and apply desugaring
  //      results map { item =>
  //        Item(Desugar.RemoveAssigns(item.task), item.path)
  //      }
  //    }
  //
  //    // Generate verilog
  //    taskItems foreach {
  //      case Item(task @ ast.Task(_, name, _), fpath) => {
  //        // Construct output file path
  //        val opath = {
  //          val subdirOpt: Option[Path] = {
  //            val pdir = fpath.parent.get
  //            if (pdir == srcdir) {
  //              None
  //            } else {
  //              Some(pdir relativize srcdir)
  //            }
  //          }
  //
  //          val oname = name + ".v"
  //
  //          subdirOpt match {
  //            case Some(subdir) => odir / subdir / oname
  //            case None         => odir / oname
  //          }
  //        }
  //
  //        // Write Verilog
  //        val makeVerilog = new MakeVerilog(moduleCatalogue)
  //        makeVerilog(task, opath)
  //      }
  //    }
  //  }
  //
  //  /////////////////////////////////////////////////////////////////////////////
  //  // Run compilation at least once
  //  /////////////////////////////////////////////////////////////////////////////
  //
  //  val cc = new CompilerContext
  //
  //  try {
  //    go(cc)
  //  } catch {
  //    case e: InternalCompilerErrorException => throw e
  //    case _: FatalErrorException            => /* Swallow */
  //  } finally {
  //    for (message <- cc.messages) {
  //      println(message)
  //    }
  //  }
  //
  //  /////////////////////////////////////////////////////////////////////////////
  //  // Decide what to do when compilation is finished
  //  /////////////////////////////////////////////////////////////////////////////
  //
  //  if (conf.time.isDefined) {
  //    // Benchmark compilation time
  //    val n = conf.time()
  //    // run 'n' times an collect the runtimes
  //    val dt = for (i <- 1 to n) yield {
  //      val t0 = System.nanoTime()
  //      val lcc = new CompilerContext
  //      lcc.note(s"Benchmarking iteration $i")
  //      go(lcc)
  //      for (message <- lcc.messages) {
  //        println(message)
  //      }
  //      (System.nanoTime() - t0) / 1e9
  //    }
  //    // Compute mean
  //    val mean = dt.sum / n
  //    // Compute 95% confidence interval using the normal distribution
  //    // This really should be based on the t distribution, but we don't
  //    // want a library dependency just for this ...
  //    val sdev = dt.map(_ - mean).map(math.pow(_, 2)).sum / (n - 1)
  //    val se = sdev / math.sqrt(n)
  //    val me = 1.96 * se
  //    println("Compilation time: %.3fs +/- %.2f%% (%.3fs, %.3fs)"
  //      format (mean, me / mean * 100, mean - me, mean + me))
  //
  //    sys exit 0
  //  } else if (conf.monitor()) {
  //    // Stay alive and wait for source changes
  //    implicit val system = ActorSystem("actorSystem")
  //    val fileMonitorActor = system.actorOf(MonitorActor(concurrency = 2))
  //
  //    def callback(path: Any): Unit = {
  //      val t0 = System.nanoTime()
  //      val lcc = new CompilerContext
  //      go(lcc)
  //      lcc.note("Compilation time: %.3fs" format ((System.nanoTime() - t0) / 1e9))
  //      for (message <- lcc.messages) {
  //        println(message)
  //      }
  //    }
  //
  //    // TODO: This is probably not safe if multiple paths change simultaneously
  //    for (path <- ipaths) {
  //      println(s"Waiting for ${path.path} to be modified (press return to quit)...")
  //      fileMonitorActor ! RegisterCallback(event    = ENTRY_MODIFY, path = Paths get path.path, callback = callback)
  //    }
  //
  //    io.StdIn.readLine()
  //    println("Quitting")
  //    system.terminate()
  //
  //    sys exit 0
  //  } else {
  //    // Normal compilation
  //
  //    // TODO: only this should write output files, and only if there are no errors
  //
  //    val exitWithError = cc.messages.exists {
  //      case _: Fatal => true
  //      case _: ICE   => true
  //      case _        => false
  //    }
  //
  //    sys exit (if (exitWithError) 1 else 0)
  //  }
}
