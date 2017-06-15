
package alogic

import java.io.File
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds._

import scala.collection.concurrent.TrieMap

import com.beachape.filemanagement.Messages._
import com.beachape.filemanagement.MonitorActor

import AstOps._
import akka.actor.ActorSystem
import scalax.file.PathMatcher._
import scalax.file.Path

// TODO: handle this in a nicer way if possible
object PortMap {
  // This must be from a concurrent collection because it is populated from multiple threads
  val portMap = new TrieMap[String, AlogicTask]()
}

object AlogicMain extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val conf = new CLIConf(args)

  val listOfFiles: List[Path] = conf.path() match {
    case IsFile(file)     => List(file)
    case IsDirectory(dir) => dir.descendants("*.alogic", 1).toList
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

    PortMap.portMap.clear()

    // Construct potentially parallel file list
    val filePaths = if (multiThreaded) listOfFiles.par else listOfFiles

    // First pass - Build AST
    val asts = filePaths flatMap {
      AParser(_, includeSearchPaths, initalDefines)
    }

    // Extract ports
    asts foreach {
      case t @ AlogicTask(name, _) => {
        if (PortMap.portMap contains name)
          Message.warning(s"$name defined multiple times")
        PortMap.portMap(name) = t; false
      }
    }

    // Second pass
    asts foreach {
      case t @ AlogicTask(name, _) => {
        // Convert to state machine
        val prog: StateProgram = t match {
          case t: NetworkTask => StateProgram(t :: Nil, 0);
          case t: VerilogTask => StateProgram(t :: Nil, 0);
          case t: FsmTask     => new MakeStates()(t)
        }

        // Remove complicated assignments and ++ and -- (MakeStates inserts some ++/--)
        val prog2 = Desugar.RemoveAssigns(prog)

        // Construct output filename
        val opath: Path = odir / (name + ".v")

        // Write Verilog
        new MakeVerilog()(prog2, opath.path)
      }
    }

  }
}
