////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds._

import com.beachape.filemanagement.Messages._
import com.beachape.filemanagement.MonitorActor

import akka.actor.ActorSystem
import scalax.file.Path
import scalax.file.PathMatcher._

object AlogicMain extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val conf = new CLIConf(args)

  val idir = conf.path()

  val listOfFiles: List[Path] = idir match {
    case IsFile(file)     => List(file)
    case IsDirectory(dir) => dir.descendants("*.alogic").toList
  }

  val odir = conf.odir()
  if (!odir.exists) {
    odir.createDirectory()
  }

  val multiThreaded = conf.parallel()

  Message.verbose = conf.verbose()

  val includeSearchPaths = conf.incdir()

  val initalDefines = conf.defs.toMap

  //////////////////////////////////////////////////////////////////////////////
  // Compile
  //////////////////////////////////////////////////////////////////////////////

  ///println("""python test.py""".!)  Example of how to run python code
  // .!! gets output
  // Process("").lines runs in background and provides a Stream[String] of output
  // Throws exception if non-zero exit code

  go

  if (conf.time.isDefined) {
    // Benchmark compilation time
    val n = conf.time()
    // run 'n' times an collect the runtimes
    val dt = for (i <- 1 to n) yield {
      Message.note(s"Benchmarking iteration $i")
      val t0 = System.nanoTime()
      go
      (System.nanoTime() - t0) / 1e9
    }
    // Compute mean
    val mean = dt.sum / n
    // Compute 95% confidence interval using the normal distribution
    // This really should be based on the t distribution, but we don't
    // want a library depencency just for this ...
    val sdev = dt.map(_ - mean).map(math.pow(_, 2)).sum / (n - 1)
    val se = sdev / math.sqrt(n)
    val me = 1.96 * se
    Message.note("Compilation time: %.3fs +/- %.2f%% (%.3fs, %.3fs)" format (mean, me / mean * 100, mean - me, mean + me))
  } else if (conf.monitor()) {
    // Stay alive and wait for source chagnes
    implicit val system = ActorSystem("actorSystem")
    val fileMonitorActor = system.actorOf(MonitorActor(concurrency = 2))
    Message.info(s"Waiting for ${conf.path().path} to be modified (press return to quit)...")
    fileMonitorActor ! RegisterCallback(
      event = ENTRY_MODIFY,
      path = Paths get conf.path().path,
      callback = { _ => go })
    io.StdIn.readLine()
    Message.info("Quitting")
    system.terminate()
  }

  sys exit (if (Message.fail) 1 else 0)

  def go() {
    // Clear caches
    Cache.clearAll()

    case class Item(task: ast.Task, path: Path)

    // Construct potentially parallel file list
    val rootPaths = if (multiThreaded) listOfFiles.par else listOfFiles

    // Build AST
    val astItems = {
      val rootItems = rootPaths flatMap { path =>
        AParser(path, includeSearchPaths, initalDefines) map { Item(_, path) }
      }

      // Extract embedded FSMs from networks
      rootItems flatMap {
        case Item(net: ast.NetworkTask, path) => MakeStages(net) match {
          case Some((network, stages)) => (network :: stages) map { Item(_, path) }
          case None                    => Nil
        }
        case item => item :: Nil
      }
    }

    // Build catalogue of all modules
    // TODO: check for multiple definitions of same module
    val moduleCatalogue = {
      astItems map { _.task } collect { case t @ ast.Task(name, _) => name -> t }
    }.toList.toMap

    // Synthesise tasks
    val taskItems = {
      val results = astItems flatMap {
        case Item(task: ast.FsmTask, path) => MakeStates(task) map { Item(_, path) }
        case item                          => Some(item)
      }

      // Flatten and apply desugaring
      results map { item =>
        Item(Desugar.RemoveAssigns(item.task), item.path)
      }
    }

    // Generate verilog
    taskItems foreach {
      case Item(task @ ast.Task(name, _), ipath) => {
        // Construct output file path
        val subdir = ipath.parent.get relativize idir

        val opath: Path = if (Option(subdir).isDefined)
          odir / subdir / (name + ".v")
        else
          odir / (name + ".v")

        // Write Verilog
        new MakeVerilog(moduleCatalogue)(task, opath)
      }
    }
  }
}
